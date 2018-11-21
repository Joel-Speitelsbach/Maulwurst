module Init exposing (..)

import Date exposing (Date)
import Array exposing (Array)
import Html exposing (Html)
import Navigation
import Task

program :
  { init          : Init model_ msg_
  , update        : Update model_ msg_
  , view          : model_ -> Html msg_
  , subscriptions : model_ -> Sub msg_
  , onNewLocation : Navigation.Location -> msg_
  }
  -> ProgramWithInit model_ msg_
program arg =
  Navigation.program (ProgramMsg << arg.onNewLocation)
    { view = \model -> view { model = model, view_ = arg.view }
    , init = init
    , update = \msg model ->
        update
          { model   = model
          , msg     = msg
          , init_   = arg.init
          , update_ = arg.update
          }
    , subscriptions = \model ->
        subscriptions
          { subscriptions_ = arg.subscriptions
          , model          = model
        }
    }


type alias ProgramWithInit model_ msg_ =
  Program Never (Model model_ msg_) (Msg msg_)


type alias Init model_ msg_ =
  { location : Navigation.Location
  , jetzt    : Date
  }
  -> ( model_, Cmd msg_ )


type alias Update model_ msg_ =
  { msg : msg_, model : model_ } -> ( model_, Cmd msg_ )



-- MODEL

type Model model_ msg_
  = Waiting (WaitingData msg_)
  | Started model_


type alias WaitingData msg_ =
  { initLocation : Navigation.Location
  , messages     : Array msg_
  }


init : Navigation.Location -> (Model model_ msg_, Cmd (Msg msg_))
init location =
  ( Waiting
      { initLocation = location
      , messages = Array.empty
      }
  , Task.perform DateMsg Date.now
  )



-- UPDATE

type Msg msg_
  = DateMsg Date
  | ProgramMsg msg_


update :
  { model   : Model model_ msg_
  , msg     : Msg msg_
  , init_   : Init model_ msg_
  , update_ : Update model_ msg_
  }
  -> (Model model_ msg_, Cmd (Msg msg_))
update arg =
  case arg.model of
    Waiting data ->
      case arg.msg of
        ProgramMsg msg_ ->
          ( Waiting
              { data
              | messages = Array.push msg_ data.messages
              }
          , Cmd.none
          )
        DateMsg date ->
          let
            (initModel_, initCmd) = (arg.init_ { location = data.initLocation, jetzt = date })
            { model_, cmds } =
              Array.foldl
                (\msg_ acc ->
                    let (newModel, cmd) = arg.update_ { msg = msg_, model = acc.model_ }
                    in { model_ = newModel, cmds = Array.push cmd acc.cmds })
                { model_ = initModel_, cmds = Array.fromList [initCmd] }
                data.messages
          in
            (Started model_, Cmd.map ProgramMsg <| Cmd.batch (Array.toList cmds))

    Started model_ ->
      case arg.msg of
        ProgramMsg msg_ ->
          arg.update_ { msg = msg_, model = model_ }
          |> Tuple.mapFirst Started
          |> Tuple.mapSecond (Cmd.map ProgramMsg)
        DateMsg date ->
          let _ = Debug.log "error: after initialization recieved DateMsg" date
          in (arg.model, Cmd.none)



-- SUBSCRIPTIONS

subscriptions :
  { subscriptions_ : model_ -> Sub msg_
  , model          : Model model_ msg_
  }
  -> Sub (Msg msg_)
subscriptions arg =
  case arg.model of
    Waiting _ -> Sub.none
    Started model_ -> Sub.map ProgramMsg (arg.subscriptions_ model_)



-- VIEW

view :
  { model : Model model_ msg_
  , view_ : model_ -> Html msg_
  }
  -> Html (Msg msg_)
view arg =
  case arg.model of
    Waiting _ -> Html.text ""
    Started model_ -> Html.map ProgramMsg (arg.view_ model_)
