module Übersicht exposing (..)

import CommonnTypes exposing (..)
import CommonTypes exposing (..)
import Data.Either exposing (..)
import Date exposing (Date)
import Datum
import DayColors as Days
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Stil exposing (Stil, scale, spacin, pading, pxx, vergr)
import Tabelle as Tab
import ToServer as ToServer


-----------------------------------------------------------------------------
----------------------- MODEL ------------------------------------------------

type alias Read =
  { lieferungen : List Lieferung
  }


type alias Model =
  { neueLieferungAngefordert : Bool
  , sortby                   : Sortby
  , anzuzeigendeBtypen       : List Bestelltyp
  , zeigePapierkorb          : Bool
  }


init : Model
init =
  { neueLieferungAngefordert = False
  , sortby =
      { kategorie = Bestelldatum
      , vorwärts = True
      }
  , anzuzeigendeBtypen = [Merchingen,Partyservice,Adelsheim]
  , zeigePapierkorb = False
  }


type alias Sortby =
  { kategorie : SortCategory
  , vorwärts : Bool
  }


type SortCategory
  = Bestelldatum
  | Lieferdatum
  | SortStatus
  | Kunde
  | SortBestelltyp


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
  = NeueLieferung
  | ChangeSort SortCategory
  | ChangeBFilter Bestelltyp Bool
  | ChangePapFilter Bool
  | GeheZu Programmansicht


update : Msg -> Model -> (Either Programmansicht Model, Cmd msg)
update msg model =
  case msg of
    NeueLieferung ->
      ( Right
          { model
          | neueLieferungAngefordert = True
          }
      , if model.neueLieferungAngefordert
        then Cmd.none
        else ToServer.send ToServer.neueLieferung
      )
    _ -> (updateNoCmd msg model, Cmd.none)


updateNoCmd : Msg -> Model -> Either Programmansicht Model
updateNoCmd msg model =
  case msg of
    GeheZu ansicht -> Left ansicht
    ChangeSort sortby -> Right
      { model
      | sortby =
          if sortby == model.sortby.kategorie
            then { kategorie = sortby, vorwärts = not model.sortby.vorwärts}
            else { kategorie = sortby, vorwärts = True}
      }
    ChangeBFilter btyp active -> Right
      { model
      | anzuzeigendeBtypen =
          List.filter (\b -> b /= btyp) model.anzuzeigendeBtypen
          ++
          if active
            then [btyp]
            else []
      }
    ChangePapFilter active -> Right
      { model
      | zeigePapierkorb = active
      }
    _ -> Right model


---------------------------------------------------------------------------
------------------------ VIEW ---------------------------------------------


view : Read -> Model -> Elem Msg
view read model =
  el Stil.Neutral [center, spacin 20] <| column Stil.Neutral [spacin 10, paddingBottom (vergr 20)] <|
    let
      space = spacin 10
      columnSizes = [110, 240, 200, 200, 100]
      currSortby = model.sortby
      sortButton sortby txt =
        let
          arrow =
            if sortby == currSortby.kategorie
              then if currSortby.vorwärts then " ↓" else " ↑"
              else ""
        in button Stil.SortButton [center, onClick (ChangeSort sortby)] (text (txt ++ arrow))
    in
      [ viewNavigationBar
      , viewFilterCheckboxes model
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
              btypActive l = List.member l.bestelltyp model.anzuzeigendeBtypen
              pap l =
                if model.zeigePapierkorb
                  then True
                  else
                    not <| inPapierkorbBool l
              filtered =
                List.filter
                  (\l -> pap l && btypActive l
                  )
                  read.lieferungen
            in sortiere currSortby filtered
         )


viewNavigationBar : Elem Msg
viewNavigationBar =
  flip (button Stil.ButtonSmall) (text "Zur Artikelübersicht -->") <|
    [ onClick <| GeheZu AnsichtArtikelübersicht, height (pxx 30) ]


viewFilterCheckboxes { anzuzeigendeBtypen, zeigePapierkorb } =
  row Stil.Neutral [ spacin 20 ] <|
    [ text "Zeige Orte "
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


lieferungReihe : Attr Msg -> List Float  -> Formatiert                           -> Elem Msg
lieferungReihe   space       columnSizes    {lieferung,lieferdatum,bestelldatum}    =
  button Stil.HiddenButton [onClick (GeheZu <| AnsichtDetails lieferung.id)] <|
    Tab.reihe [space] columnSizes
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


type alias Formatiert =
  { lieferung    : Lieferung
  , lieferdatum  : Elem Msg
  , bestelldatum : Elem Msg
  }


withDayColors :
  { getDate     : Lieferung  -> Maybe Date
  , getEl       : Formatiert -> Elem Msg
  , setEl       : Elem Msg   -> Formatiert -> Formatiert
  , lieferungen : List Lieferung
  }
  -> List Formatiert
withDayColors { getDate, getEl, setEl, lieferungen } =
  List.map2
    (\farbStil fmt ->
        setEl (el farbStil [] <| getEl fmt) fmt
    )
    (Days.farbSegmente <| List.map getDate lieferungen)
    (List.map formatDefault lieferungen)


formatDefault : Lieferung -> Formatiert
formatDefault lieferung =
  { lieferung = lieferung
  , lieferdatum =
      let
        txt = Datum.toStr lieferung.lieferdatum
      in textCenter txt
  , bestelldatum =
      textCenter (Datum.toStr <| Datum.Datum lieferung.bestelldatum)
  }


sortiere : Sortby -> List Lieferung -> List Formatiert
sortiere {vorwärts,kategorie} lieferungen =
  let
    sortiert =
      case kategorie of
        Bestelldatum ->
          withDayColors
            { getDate     = .bestelldatum >> Just
            , getEl       = .bestelldatum
            , setEl       = \el fmt -> { fmt | bestelldatum = el }
            , lieferungen = List.sortBy (.bestelldatum >> Date.toTime >> (\t -> -t)) lieferungen
            }
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
              { getDate     =
                  .lieferdatum >> \dat ->
                    case dat of
                      Datum.Datum dat -> Just dat
                      Datum.DatumStr _ -> Nothing

              , getEl       = .lieferdatum
              , setEl       = (\el fmt -> { fmt | lieferdatum = el})
              , lieferungen = (List.sortBy by filtered)
              }
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
