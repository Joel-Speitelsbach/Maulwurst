module Main exposing (..)

import CommonnTypes exposing (..)
import CommonTypes exposing (..)
import Data.Either exposing (..)
import Date exposing (Date)
import Details
import Element exposing (..)
import Element.Attributes exposing (..)
import FromServer as FromServer exposing (ServerMsg(..))
import Html exposing (Html)
import Local
import Platform.Sub as Sub
import Stil exposing (pading,Stil)
import Time exposing (Time, minute)
import Übersicht
import WebSocket

main = Html.program {view=view, init=init, update=update, subscriptions=subscriptions}


-----------------------------------------------------------------------------
----------------------- MODEL ------------------------------------------------

type alias Model =
  { lieferungen           : List Lieferung
  , übersichtZustand      : Übersicht.Model
  , ansicht               : Ansicht
  , jetztM                : Maybe Time
  , letzteServerNachricht : Time
  }

type Ansicht
  = Übersicht
  | Details Details.Ansicht

init : (Model, Cmd Msg)
init =
  ( { lieferungen = []
    , übersichtZustand =
        { neueLieferungAngefordert = False
        , sortby =
            { kategorie = Übersicht.Bestelldatum
            , vorwärts = True
            }
        , anzuzeigendeBtypen = [Merchingen,Partyservice,Adelsheim]
        , zeigePapierkorb = False
        }
    , ansicht = Übersicht
    , jetztM = Nothing
    , letzteServerNachricht = 0
    }
  , Cmd.none
  )

connectionActive : Model -> Bool
connectionActive { jetztM, letzteServerNachricht } =
  Maybe.map
    (\jetzt ->
      jetzt - letzteServerNachricht < 7 * Time.second
    )
    jetztM
  |> Maybe.withDefault False

---------------------------------------------------------------------
-------------------------- UPDATE ---------------------------------

type Msg
  = DetailsMsg Details.Msg
  | ÜbersichtMsg Übersicht.Msg
  | FromServer ServerMsg
  | NeueZeit Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DetailsMsg msg ->
      case model.ansicht of
        Übersicht -> (model, Cmd.none)
        Details ansicht ->
          let
            details = Details.NotLeave
              { lieferungen = model.lieferungen
              , ansicht = ansicht
              }
            (new_details, cmd) = Details.update msg details
            newModel = case new_details of
              Details.Leave ->
                { model
                | ansicht = Übersicht
                }
              Details.NotLeave new_details_ ->
                { model
                | ansicht = Details new_details_.ansicht
                }
          in (newModel, Cmd.map DetailsMsg cmd)
    ÜbersichtMsg msg ->
      Tuple.mapFirst
        (\res -> case res of
          Left (Übersicht.GeheZuDetails id) ->
            { model
            | ansicht = Details
                { modus = Details.NormalAnsicht
                , liefer_id = id
                }
            }
          Right ansicht ->
            { model
            | übersichtZustand = ansicht
            }
        )
        (Übersicht.update msg model.übersichtZustand)
    _ -> (updateNoCmd msg model, Cmd.none)

updateNoCmd msg model =
  case msg of
    FromServer msg ->
      reactServer msg model
      |> (\m ->
            { m
            | letzteServerNachricht = Maybe.withDefault 0 model.jetztM
            }
         )
    NeueZeit zeit ->
      { model | jetztM = Just zeit }
    _                  -> model

reactServer msg model =
  case msg of
    ServerChangeLieferung lieferung ->
      { model
      | lieferungen = changeLieferungen lieferung model.lieferungen
      }
    ServerZeigeNeueLieferung id ->
      { model
      | ansicht =
          Details
            { liefer_id = id
            , modus = Details.NormalAnsicht
            }
      , übersichtZustand =
          let curr = model.übersichtZustand
          in
            { curr
            | neueLieferungAngefordert = False
            }
      }
    ServerLöscheBestellung lid bid ->
      { model
      | lieferungen = List.map (löscheBestellung bid) model.lieferungen
      , ansicht =
          case model.ansicht of
            Details d ->
              if lid == d.liefer_id
              then Details { d | modus = Details.Reloading }
              else Details d
            a         -> a
      }
    ServerLöscheLieferung id ->
      löscheLieferung id model
    ServerSetzeAllesZurück ->
      { model | lieferungen = [] }
    ServerHerzschlag -> model
    MalformedMsg str ->
      let l = Debug.log ("malformed server msg: " ++ str) ()
      in  model

changeLieferungen : Lieferung -> List Lieferung -> List Lieferung
changeLieferungen lieferung lieferungen =
  lieferung :: List.filter (\l -> l.id /= lieferung.id) lieferungen


löscheBestellung : Int -> Lieferung -> Lieferung
löscheBestellung id lieferung =
  { lieferung
  | bestellungen = List.filter (\b -> b.id /= id) lieferung.bestellungen
  }

löscheLieferung : Int -> Model -> Model
löscheLieferung id model =
  { model
  | lieferungen = List.filter (\l -> l.id /= id) model.lieferungen
  , ansicht =
      case model.ansicht of
        Details d ->
          if d.liefer_id == id
          then Übersicht
          else model.ansicht
        _ -> model.ansicht
  }

--------------------------------------------------------------------------------
------------------------ SUBSCRIPTIONS ------------------------------------------

subscriptions model =
  let
    websocket = WebSocket.listen Local.serverUrl (FromServer << FromServer.parseServerMsg)
    zeit = Time.every 1000 NeueZeit
    details =
      case model.ansicht of
        Details details -> Sub.map DetailsMsg <| Details.subscriptions details
        _ -> Sub.none
  in
    Sub.batch
      [ websocket
      , zeit
      , details
      ]


----------------------------------------------------------------------------
------------------------------ VIEW ----------------------------------------

view : Model -> Html Msg
view model =
  let
    übersicht =
      Element.map ÜbersichtMsg <|
        Übersicht.view
          { lieferungen = model.lieferungen
          }
          model.übersichtZustand
  in
    viewport Stil.stylesheet <|
      if connectionActive model then
        case model.ansicht of
          Übersicht -> übersicht
          Details ansicht ->
            Details.view
              { lieferungen = model.lieferungen
              , ansicht = ansicht
              }
            |> Maybe.map (Element.map DetailsMsg)
            |> Maybe.withDefault übersicht
      else viewConnecting model

viewConnecting : Model -> Element Stil var msg
viewConnecting model =
  el Stil.Big [center,verticalCenter] <|
    text <| "warte auf Server (" ++ toString (List.length model.lieferungen)  ++ ")..."
