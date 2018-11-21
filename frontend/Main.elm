module Main exposing (..)

import Artikelübersicht
import CommonnTypes exposing (..)
import CommonTypes exposing (..)
import Data.Either exposing (..)
import Date exposing (Date)
import Datum
import Details
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import FromServer as FromServer exposing (ServerMsg(..))
import Html exposing (Html)
import Init
import Local
import Navigation
import Platform.Sub as Sub
import Stil exposing (Stil, spacin, pading, vergr)
import Time exposing (Time, minute)
import Übersicht
import WebSocket


main = Init.program
  { view          = view
  , init          = init
  , update        = update
  , subscriptions = subscriptions
  , onNewLocation = NeueUrl
  }


-----------------------------------------------------------------------------
----------------------- MODEL ------------------------------------------------


type alias Model =
  { lieferungen           : List Lieferung
  , übersichtZustand      : Übersicht.Model
  , artikelÜbrZustand     : Artikelübersicht.Model
  , ansicht               : Ansicht
  , letzteÜbersicht       : Ansicht
  , jetzt                 : Date
  , letzteServerNachricht : Date
  , aktuelleUrl           : String
  , zeitraumfilter        : Zeitraumfilter
  }


type Ansicht
  = Übersicht
  | Details { model : Details.Model, liefer_id : Int }
  | Artikelübersicht


init : { location : Navigation.Location, jetzt : Date } -> (Model, Cmd Msg)
init { location, jetzt } =
  ( { lieferungen           = []
    , übersichtZustand      = Übersicht.init
    , artikelÜbrZustand     = Artikelübersicht.init
    , ansicht               = startAnsicht
    , letzteÜbersicht       = startAnsicht
    , jetzt                 = jetzt
    , letzteServerNachricht = Date.fromTime 0
    , aktuelleUrl           = location.pathname
    , zeitraumfilter        = initZeitraumfilter { jetzt = jetzt }
    }
  , Navigation.newUrl navigationForwardStr
  )


initZeitraumfilter : { jetzt : Date } -> Zeitraumfilter
initZeitraumfilter { jetzt } =
  let
    von = Datum.nextWeekday { today = jetzt, weekday = Date.Mon }
    bis =
      Date.fromTime <|
        Date.toTime (Datum.nextWeekday { today = von, weekday = Date.Sun })
        + Time.hour * 24
  in
    { von = Datum.Datum <| von
    , bis = Datum.Datum <| bis
    }


type alias Zeitraumfilter = { von : Datum.Model, bis : Datum.Model }


startAnsicht = Artikelübersicht
-- startAnsicht = Details { model = { ansicht = Details.Druckansicht }, liefer_id = 1 }


navigationForwardStr = "/Main.elm/"


connectionActive : Model -> Bool
connectionActive { jetzt, letzteServerNachricht } =
  Date.toTime jetzt
  - Date.toTime letzteServerNachricht
  <
  7 * Time.second



filtereLieferungen :
  { zeitraum    : Zeitraumfilter
  , lieferungen : List Lieferung
  }
  -> List Lieferung
filtereLieferungen arg =
  case (arg.zeitraum.von, arg.zeitraum.bis) of
    (Datum.Datum von, Datum.Datum bis) ->
      List.filter
        (\lieferung ->
            case lieferung.lieferdatum of
              Datum.DatumStr _ -> False
              Datum.Datum date ->
                Date.toTime von < Date.toTime date
                &&
                Date.toTime date < Date.toTime bis)
        arg.lieferungen
    (Datum.DatumStr "", _) -> arg.lieferungen
    (_, Datum.DatumStr "") -> arg.lieferungen
    _ -> []



---------------------------------------------------------------------
-------------------------- UPDATE ---------------------------------

type Msg
  = DetailsMsg Details.Msg
  | ÜbersichtMsg Übersicht.Msg
  | ArtikelübersichtMsg Artikelübersicht.Msg
  | FromServer ServerMsg
  | NeueZeit Date
  | NeueUrl Navigation.Location
  | ÄndereZeitfilter ÄndereZeitfilter


type ÄndereZeitfilter
  = Von Datum.Msg
  | Bis Datum.Msg


update : { msg : Msg, model : Model } -> (Model, Cmd Msg)
update { msg, model } =
  case msg of
    DetailsMsg msg ->
      case model.ansicht of
        Details details ->
          case
            ixLieferung model.lieferungen details.liefer_id
            |> Maybe.map (\lieferung ->
                  reactDetails
                    { lieferung = lieferung
                    , msg = msg
                    , model = model
                    , details_model = details.model
                    })
          of Just x  -> x
             Nothing -> let _ = Debug.log ("error: Lieferung does not exist, id: ") details.liefer_id
                        in (model, Cmd.none)
        _ -> let _ = Debug.log "error: recieved DetailsMsg in" model.ansicht
             in (model, Cmd.none)
    ÜbersichtMsg msg ->
      Tuple.mapFirst
        (\res -> case res of
          Left ansicht -> wechsleAnsicht (macheAnsicht ansicht) model
          Right neuerZustand ->
            { model
            | übersichtZustand = neuerZustand
            }
        )
        (Übersicht.update msg model.übersichtZustand)
    ArtikelübersichtMsg msg ->
      let
        update = Artikelübersicht.update msg model.artikelÜbrZustand
        newModel = case update of
          Left neueAnsicht ->
             wechsleAnsicht (macheAnsicht neueAnsicht) model
          Right neuerZustand ->
            { model | artikelÜbrZustand = neuerZustand }
      in (newModel, Cmd.none)
    NeueUrl { pathname } ->
      let
        hasSufix : String -> String -> Bool
        hasSufix str prefix = String.right (String.length prefix) str == prefix
      in
        if hasSufix pathname navigationForwardStr
          then (model, Cmd.none)
          else
            ( wechsleAnsicht model.letzteÜbersicht model
            , Navigation.newUrl navigationForwardStr
            )
    ÄndereZeitfilter änderung ->
      ( { model
        | zeitraumfilter =
            let
              zeitraum = model.zeitraumfilter
              today = model.jetzt
              von = case zeitraum.von of
                Datum.Datum date -> date
                Datum.DatumStr _ -> today
            in
              case änderung of
                Von msg ->
                  { zeitraum
                  | von = Datum.update { model = zeitraum.von, today = today, msg = msg }
                  }
                Bis msg ->
                  { zeitraum
                  | bis = Datum.update { model = zeitraum.bis, today = von, msg = msg }
                  }
        }
      , Cmd.none
      )
    _ -> (updateNoCmd msg model, Cmd.none)


updateNoCmd msg model =
  case msg of
    FromServer msg ->
      reactServer msg model
      |> (\model_ ->
            { model_
            | letzteServerNachricht = model.jetzt
            })
    NeueZeit zeit -> { model | jetzt = zeit }
    _ -> model


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
            , model = Details.init
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
              then Details { d | model = { ansicht = Details.Reloading } }
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


reactDetails :
  { msg : Details.Msg
  , model : Model
  , lieferung : Lieferung
  , details_model : Details.Model
  }
  -> (Model, Cmd Msg)
reactDetails { msg, model, lieferung, details_model } =
  let
    (update, cmd) =
      Details.update
        { lieferung = lieferung
        , msg = msg
        , model = details_model
        , today = model.jetzt
        }
    newModel = case update of
      Details.Leave ->
        { model
        | ansicht = model.letzteÜbersicht
        }
      Details.NotLeave update_ ->
        { model
        | ansicht = Details
            { model = update_
            , liefer_id = lieferung.id
            }
        }
  in (newModel, Cmd.map DetailsMsg cmd)


macheAnsicht : Programmansicht -> Ansicht
macheAnsicht programmansicht =
  case programmansicht of
    AnsichtDetails liefer_id ->
      Details
        { model = Details.init
        , liefer_id = liefer_id
        }
    AnsichtÜbersicht        -> Übersicht
    AnsichtArtikelübersicht -> Artikelübersicht


wechsleAnsicht : Ansicht -> Model -> Model
wechsleAnsicht ansicht model =
  { model
  | ansicht = ansicht
  , letzteÜbersicht =
      case ansicht of
        Details _ -> model.ansicht
        _         -> model.letzteÜbersicht
  }


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
    zeit = Time.every 1000 (NeueZeit << Date.fromTime)
    details =
      case model.ansicht of
        Details details -> Sub.map DetailsMsg <| Details.subscriptions { model = details.model }
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
    filtereLieferungen_ () =
      filtereLieferungen
        { lieferungen = model.lieferungen
        , zeitraum = model.zeitraumfilter
        }
    viewZeitraumFilter_ () =
      viewZeitraumFilter
          { von = model.zeitraumfilter.von
          , bis = model.zeitraumfilter.bis
          }
    übersicht () =
      column Stil.Neutral []
        [ viewZeitraumFilter_ ()
        , Element.map ÜbersichtMsg <|
            Übersicht.view
              { lieferungen = filtereLieferungen_ () }
              model.übersichtZustand
        ]
  in
    viewport Stil.stylesheet <|
      if not <| connectionActive model
      then viewConnecting model
      else
        case model.ansicht of
          Übersicht -> übersicht ()
          Artikelübersicht ->
            column Stil.Neutral []
              [ viewZeitraumFilter_ ()
              , Artikelübersicht.view
                  { lieferungen = filtereLieferungen_ ()
                  , model = model.artikelÜbrZustand
                  }
                |> Element.map ArtikelübersichtMsg
              ]
          Details details ->
            ixLieferung model.lieferungen details.liefer_id
            |> Maybe.map (\lieferung ->
                 Details.view
                   { lieferung = lieferung
                   , ansicht = details.model.ansicht
                   }
                 |> Element.map DetailsMsg)
            |> Maybe.withDefault (übersicht ())


viewConnecting : Model -> Elem msg
viewConnecting model =
  el Stil.Big [center,verticalCenter] <|
    text <| "warte auf Server (" ++ toString (List.length model.lieferungen)  ++ ")..."


viewZeitraumFilter : { von : Datum.Model, bis : Datum.Model } -> Elem Msg
viewZeitraumFilter arg =
  row Stil.Neutral [spacin 40, pading 20, center]
    [ text "Zeige Zeitraum"
    , row Stil.Neutral [spacin 10]
        [ Datum.viewPickDate { label = Just "von:", model = arg.von }
          |> Element.map (\msg -> ÄndereZeitfilter (Von msg))
        , Datum.viewPickDate { label = Just "bis:", model = arg.bis }
          |> Element.map (\msg -> ÄndereZeitfilter (Bis msg))
        ]
    ]



-- MISC

viewTextInput : { label : String, inhalt : String, onChange : String -> msg } -> Elem msg
viewTextInput arg =
  Input.text Stil.TextField []
    { onChange = arg.onChange
    , value    = arg.inhalt
    , label    = Input.labelLeft <| text arg.label
    , options  = []
    }
