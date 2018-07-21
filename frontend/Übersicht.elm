module Übersicht exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Date exposing (Date)
import Stil exposing (Stil, scale, spacin, pading, pxx)
import Types exposing (..)
import Tabelle as Tab
import Html exposing (Html)
import ToServer as ToServer

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
  lieferung.lieferdatum == ""
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

type alias Elem variation = Element Stil variation Msg
type alias Attrs variation = List (Attribute variation Msg)

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
            , reloading = False
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

view : Model -> Html Msg
view model =
  viewport Stil.stylesheet <| el Stil.Neutral [center, pading 20] <|
    el Stil.Neutral [ spacin 20] <|
       tabelleÜbersicht model

tabelleÜbersicht : Model -> Elem var
tabelleÜbersicht model =
  column Stil.Neutral [spacin 10] <|
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
            in sortiere currSortby.vorwärts currSortby.kategorie filtered
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

lieferungReihe space columnSizes lieferung =
  button Stil.HiddenButton [onClick (ZeigeDetails lieferung.id)] <| Tab.reihe [space] columnSizes
    [ el (Stil.Btyp lieferung.bestelltyp) [] << el Stil.Neutral [verticalCenter,center] << text <|
        bestelltypString lieferung.bestelltyp
    , textCenter (lieferung.kundenname)
    , textCenter (viewDate lieferung.bestelldatum)
    , let
        txt =
          Result.map viewDate (Date.fromString lieferung.lieferdatum)
          |> Result.withDefault lieferung.lieferdatum
      in textCenter txt
    , viewStatus (lieferStatus lieferung)
    ]

textCenter = el Stil.Neutral [] << el Stil.Neutral [verticalCenter,center] << text

viewStatus status =
  el (Stil.Stat status) [] <| el Stil.Neutral [verticalCenter,center] <|
    text <| statusString status

sortiere : Bool -> SortCategory -> List Lieferung -> List Lieferung
sortiere vorwärts sortby lieferungen =
  let
    sortiert =
      case sortby of
        Bestelldatum -> List.sortBy (.bestelldatum >> Date.toTime >> (\t -> -t)) lieferungen
        Lieferdatum ->
          let
            by l =
                Date.fromString l.lieferdatum
                |> Result.map Date.toTime
                |> Result.withDefault 0
                |> (\t -> -t)
            filtered =
              List.filter
                (\l ->
                  Result.toMaybe (Date.fromString l.lieferdatum) /= Nothing
                )
                lieferungen
          in List.sortBy by filtered
        SortStatus ->
          let
            by l =
              case lieferStatus l of
                Just Neu           -> 0
                Just InBearbeitung -> 1
                Just Fertig        -> 2
                Nothing            -> 3
          in List.sortBy by lieferungen
        Kunde -> List.sortBy .kundenname lieferungen
        SortBestelltyp ->
          let
            by l =
              case l.bestelltyp of
                Adelsheim    -> 0
                Merchingen   -> 1
                Partyservice -> 2
          in List.sortBy by lieferungen
  in
    if vorwärts then sortiert else List.reverse sortiert

viewDate : Date -> String
viewDate datum =
  let
    zahlen =
      List.map ((|>) datum)
        [ toString << Date.day
        , toString << Date.month
        , toString << Date.year
        , toString << Date.hour
        , String.padLeft 2 '0' << toString << Date.minute
        ]
      ++ [""]
    format = String.split "%" "%. % %, %:% Uhr"
  in String.concat <| List.map2 (++) format zahlen

---------------------------------------
------------ Löschdialog ----------------

viewLöschDialog löschen zurück =
  row Stil.Neutral []
    [ button Stil.Button [onClick löschen] (text "In Papierkorb legen")
    , button Stil.Button [onClick zurück] (text "Behalten")
    ]
