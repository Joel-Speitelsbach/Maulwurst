module Main exposing (..)

import Types exposing (..)
import Time exposing (minute)
import Platform.Sub as Sub
import Details
import Übersicht
import Html exposing (Html)
-- import Html.Attributes as HtmlA
import Date exposing (Date)
import FromServer as FromServer exposing (ServerMsg(..))
import WebSocket

main = Html.program {view=view, init=(init,Cmd.none), update=update, subscriptions=subscriptions}

-----------------------------------------------------------------------------
----------------------- MODEL ------------------------------------------------

init : Model
init =
  { lieferungen = []
  , übersichtZustand =
      { neueLieferungAngefordert = False
      , sortby =
          { kategorie = Bestelldatum
          , vorwärts = True
          }
      , anzuzeigendeBtypen = [Merchingen,Partyservice,Adelsheim]
      , zeigePapierkorb = False
      }
  , ansicht = Übersicht
  }

---------------------------------------------------------------------
-------------------------- UPDATE ---------------------------------

type Msg
  = DetailsMsg Details.Msg
  | ÜbersichtMsg Übersicht.Msg
  | FromServer ServerMsg

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    DetailsMsg   msg -> Details.update msg model
    ÜbersichtMsg msg -> Übersicht.update msg model
    _                -> (updateNoCmd msg model, Cmd.none)

updateNoCmd msg model =
  case msg of
    FromServer   msg   -> reactServer msg model
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
            { id = id
            , reloading = False
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
            Details d -> if lid == d.id then Details { d | reloading = True }
                                        else Details d
            a         -> a
      }
    ServerLöscheLieferung id ->
      löscheLieferung id model
    ServerSetzeAllesZurück ->
      { model | lieferungen = [] }
    MalformedMsg str ->
      let l = Debug.log "malformed server msg" str
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
          if d.id == id
          then Übersicht
          else model.ansicht
        _ -> model.ansicht
  }
--------------------------------------------------------------------------------
------------------------ SUBSCRIPTIONS ------------------------------------------

subscriptions model =
  let
    websocket = WebSocket.listen serverUrl (FromServer << FromServer.parseServerMsg)
  in
    Sub.batch [websocket, Sub.map DetailsMsg <| Details.subscriptions model]

----------------------------------------------------------------------------
------------------------------ VIEW ----------------------------------------

view : Model -> Html Msg
view model =
  let übersicht = Html.map ÜbersichtMsg (Übersicht.view model)
  in
    case model.ansicht of
      Übersicht -> übersicht
      Details ansicht ->
        Details.view ansicht model.lieferungen
        |> Maybe.map (Html.map DetailsMsg)
        |> Maybe.withDefault übersicht
