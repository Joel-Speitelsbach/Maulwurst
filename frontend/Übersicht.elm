module Übersicht exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Date exposing (Date)
import Stil exposing (Stil, scale, spacin, pading, pxx)
import Types exposing (..)
import CommonTypes exposing (..)
import Tabelle as Tab
import Html exposing (Html)
import ToServer as ToServer
import Datum
import DayColors as Days

-----------------------------------------------------------------------------
----------------------- MODEL ------------------------------------------------

lieferStatus : Lieferung -> Maybe Status
lieferStatus {bestellungen, inPapierkorb} =
  let
    stati = List.map .status bestellungen
    fertig = List.all identity
      [ List.isEmpty <| List.filter (\s -> s /= Fertig) stati
      , not <| List.isEmpty stati
      ]
    inBearbeitung = not <| List.isEmpty <| List.filter (\s -> s /= Neu) stati
    status =
      if      fertig        then Fertig
      else if inBearbeitung then InBearbeitung
                            else Neu
  in
    case inPapierkorb of
      Just _  -> Nothing
      Nothing -> Just status

lieferungLeer : Lieferung -> Bool
lieferungLeer lieferung =
  lieferung.lieferdatum == Datum.DatumStr ""
  && lieferung.kundenname == ""
  && lieferung.bestellungen == []

---------------------------------------------------------------------
-------------------------- UPDATE ---------------------------------

type Msg
  = ZeigeDetails Int
  | NeueLieferung
  | ChangeSort SortCategory
  | ChangeBFilter Bestelltyp Bool
  | ChangePapFilter Bool

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    NeueLieferung ->
      ( { model
        | übersichtZustand =
            let curr = model.übersichtZustand
            in
              { curr
              | neueLieferungAngefordert = True
              }
        }
      , if model.übersichtZustand.neueLieferungAngefordert
        then Cmd.none
        else ToServer.send ToServer.neueLieferung
      )
    _ -> (updateNoCmd msg model, Cmd.none)

updateNoCmd : Msg -> Model -> Model
updateNoCmd msg model =
  case msg of
    ZeigeDetails id ->
      { model
      | ansicht =
          Details
            { id = id
            , modus = DetailsNormal
            }
      }
    ChangeSort sortby ->
      { model
      | übersichtZustand =
          let curr = model.übersichtZustand
          in
            { curr
            | sortby =
                if sortby == curr.sortby.kategorie
                  then { kategorie = sortby, vorwärts = not curr.sortby.vorwärts}
                  else { kategorie = sortby, vorwärts = True}
            }
      }
    ChangeBFilter btyp active ->
      { model
      | übersichtZustand =
          let curr = model.übersichtZustand
          in
            { curr
            | anzuzeigendeBtypen =
                List.filter (\b -> b /= btyp) curr.anzuzeigendeBtypen
                ++
                if active
                  then [btyp]
                  else []
            }
      }
    ChangePapFilter active ->
      { model
      | übersichtZustand =
          let curr = model.übersichtZustand
          in
            { curr
            | zeigePapierkorb = active
            }
      }
    _ -> model

---------------------------------------------------------------------------
------------------------ VIEW ---------------------------------------------

type alias Elem var = Element Stil var Msg
type alias Attr var = Attribute var Msg
type alias Attrs var = List (Attr var)

view : Model -> Elem var
view model =
  el Stil.Neutral [center, spacin 20] <| column Stil.Neutral [spacin 10] <|
    let
      space = spacin 10
      columnSizes = [110, 240, 200, 200, 100]
      currSortby = model.übersichtZustand.sortby
      sortButton sortby txt =
        let
          arrow =
            if sortby == currSortby.kategorie
              then if currSortby.vorwärts then " ↓" else " ↑"
              else ""
        in button Stil.SortButton [center, onClick (ChangeSort sortby)] (text (txt ++ arrow))
    in
      [ viewFilterCheckboxes model.übersichtZustand
      , Tab.reihe [space] columnSizes
          [ sortButton SortBestelltyp "Bestelltyp"
          , sortButton Kunde "Kunde"
          , sortButton Bestelldatum "Bestelldatum"
          , sortButton Lieferdatum "Lieferdatum"
          , sortButton SortStatus "Status"
          ]
      , flip (button Stil.ButtonSmall) (text "Neue Bestellung...") <|
          [ onClick  NeueLieferung, height (pxx 30) ]
      ]
      ++ (List.map (lieferungReihe space columnSizes) <|
            let
              btypActive l = List.member l.bestelltyp model.übersichtZustand.anzuzeigendeBtypen
              pap l =
                if model.übersichtZustand.zeigePapierkorb
                  then True
                  else
                    not <| inPapierkorbBool l
              filtered =
                List.filter
                  (\l -> pap l && btypActive l
                  )
                  model.lieferungen
            in sortiere currSortby filtered
         )

viewFilterCheckboxes { anzuzeigendeBtypen, zeigePapierkorb }=
  row Stil.Neutral [ spacin 20 ] <|
    [ text "Zeige folgende an: "
    , viewBFilterCheckboxes anzuzeigendeBtypen
    , viewPapFilterCheckbox zeigePapierkorb
    ]

viewBFilterCheckboxes anzuzeigendeBtypen  =
  let
    checkbox btyp =
      Input.checkbox Stil.Neutral []
        { onChange = ChangeBFilter btyp
        , checked = List.member btyp anzuzeigendeBtypen
        , label = el Stil.Neutral [] <| text (bestelltypString btyp)
        , options = []
        }
  in
    row Stil.Neutral [spacin 20 ] <|
      List.map checkbox [Merchingen,Adelsheim,Partyservice]

viewPapFilterCheckbox zeigePapierkorb =
  Input.checkbox Stil.Neutral [spacin 10]
    { onChange = ChangePapFilter
    , checked = zeigePapierkorb
    , label = el Stil.Neutral [] <| text "Papierkorb"
    , options = []
    }

lieferungReihe : Attr var -> List Float -> Formatiert var -> Elem var
lieferungReihe space columnSizes {lieferung,lieferdatum,bestelldatum} =
  button Stil.HiddenButton [onClick (ZeigeDetails lieferung.id)] <| Tab.reihe [space] columnSizes
    [ el (Stil.Btyp lieferung.bestelltyp) [] << el Stil.Neutral [verticalCenter,center] << text <|
        bestelltypString lieferung.bestelltyp
    , textCenter (lieferung.kundenname)
    , bestelldatum
    , lieferdatum
    , viewStatus (lieferStatus lieferung)
    ]

textCenter = el Stil.Neutral [] << el Stil.Neutral [verticalCenter,center] << text

viewStatus status =
  el (Stil.Stat status) [] <| el Stil.Neutral [verticalCenter,center] <|
    text <| statusString status

type alias Formatiert var =
  { lieferung : Lieferung
  , lieferdatum : Elem var
  , bestelldatum : Elem var
  }

-- formatiere : Sortby -> List Lieferung -> List (Formatiert var)
-- formatiere sortby lieferungen =

withDayColors
  :  (Lieferung -> Maybe Date)
  -> (Formatiert var -> Elem var)
  -> (Elem var -> Formatiert var -> Formatiert var)
  -> List Lieferung
  -> List (Formatiert var)
withDayColors getDate getEl setEl lieferungen =
  List.map2
    (\farbStil fmt ->
        setEl (el farbStil [] <| getEl fmt) fmt
    )
    (Days.farbSegemente <| List.map getDate lieferungen)
    (List.map formatDefault lieferungen)

formatDefault : Lieferung -> Formatiert var
formatDefault lieferung =
  { lieferung = lieferung
  , lieferdatum =
      let
        txt = Datum.format lieferung.lieferdatum
      in textCenter txt
  , bestelldatum =
      textCenter (Datum.format <| Datum.Datum lieferung.bestelldatum)
  }

sortiere : Sortby -> List Lieferung -> List (Formatiert var)
sortiere {vorwärts,kategorie} lieferungen =
  let
    sortiert =
      case kategorie of
        Bestelldatum ->
          withDayColors
            (.bestelldatum >> Just)
            .bestelldatum
            (\el fmt -> { fmt | bestelldatum = el}) <|
            List.sortBy (.bestelldatum >> Date.toTime >> (\t -> -t)) lieferungen
        Lieferdatum ->
          let
            by lief =
              case lief.lieferdatum of
                Datum.Datum datum ->  -(Date.toTime datum)
                Datum.DatumStr str -> 0
            filtered =
              List.filter
                (\lief ->
                   case lief.lieferdatum of
                     Datum.Datum _ -> True
                     Datum.DatumStr _ -> False
                )
                lieferungen
          in
            withDayColors
              (.lieferdatum >> \dat ->
                case dat of
                  Datum.Datum dat -> Just dat
                  Datum.DatumStr _ -> Nothing
              )
              .lieferdatum
              (\el fmt -> { fmt | lieferdatum = el})
              (List.sortBy by filtered)
        SortStatus ->
          let
            by l =
              case lieferStatus l of
                Just Neu           -> 0
                Just InBearbeitung -> 1
                Just Fertig        -> 2
                Nothing            -> 3
          in List.map formatDefault <| List.sortBy by lieferungen
        Kunde -> List.map formatDefault <| List.sortBy .kundenname lieferungen
        SortBestelltyp ->
          let
            by l =
              case l.bestelltyp of
                Adelsheim    -> 0
                Merchingen   -> 1
                Partyservice -> 2
          in List.map formatDefault <| List.sortBy by lieferungen
  in
    if vorwärts then sortiert else List.reverse sortiert
