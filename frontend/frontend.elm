module Main exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Stil exposing (pading,Stil)
import Types exposing (..)
import CommonTypes exposing (..)
import Time exposing (minute)
import Platform.Sub as Sub
import Details
import Übersicht
import Html exposing (Html)
import Date exposing (Date)
import Time exposing (Time)
import FromServer as FromServer exposing (ServerMsg(..))
import WebSocket

main = Html.program {view=view, init=init, update=update, subscriptions=subscriptions}


-----------------------------------------------------------------------------
----------------------- MODEL ------------------------------------------------

init : (Model, Cmd Msg)
init =
  ( { lieferungen = []
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
    , jetzt = 10 * Time.minute
    , letzteServerNachricht = 0
    }
  , Cmd.none
  )

connectionActive : Model -> Bool
connectionActive { jetzt, letzteServerNachricht } =
  jetzt - letzteServerNachricht < 7 * Time.second

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
      let (newModel, cmd) = Details.update msg model
      in (newModel, Cmd.map DetailsMsg cmd)
    ÜbersichtMsg msg -> Übersicht.update msg model
    _ -> (updateNoCmd msg model, Cmd.none)

updateNoCmd msg model =
  case msg of
    FromServer msg ->
      reactServer msg model
      |> (\m ->
            { m
            | letzteServerNachricht = model.jetzt
            }
         )
    NeueZeit zeit ->
      { model | jetzt = zeit}
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
            , modus = DetailsNormal
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
            Details d -> if lid == d.id then Details { d | modus = Reloading }
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
    zeit = Time.every 1000 NeueZeit
  in
    Sub.batch [websocket, zeit, Sub.map DetailsMsg <| Details.subscriptions model]


----------------------------------------------------------------------------
------------------------------ VIEW ----------------------------------------

view : Model -> Html Msg
view model =
  let übersicht = Element.map ÜbersichtMsg (Übersicht.view model)
  in
    viewport Stil.stylesheet <|
      if connectionActive model then
        case model.ansicht of
          Übersicht -> übersicht
          Details ansicht ->
            Details.view ansicht model.lieferungen
            |> Maybe.map (Element.map DetailsMsg)
            |> Maybe.withDefault übersicht
      else viewConnecting

viewConnecting : Element Stil var msg
viewConnecting =
  el Stil.Big [center,verticalCenter] <|
    text "warte auf Server..."
