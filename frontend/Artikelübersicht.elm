module Artikelübersicht
  exposing
    ( view
    , Msg
    , Model
    , init
    , update
    )

import CommonTypes exposing (..)
import Element exposing (..)
import Element.Attributes exposing (center,height,paddingBottom)
import Element.Events exposing (onClick)
import Element.Input as Input
import Stil
import Data.Either exposing (..)
import Stil exposing (Stil, scale, spacin, pading, pxx, vergr)
import Tabelle as Tab
import Datum
import Debug
import Date exposing (Date)
import Stil
import DayColors
import Regex


-------------------------------------------------------------------------
-------------------------------- MODEL -----------------------------------

type alias Model =
  { sortiere : Eigenschaft
  , sortiereVorwärts : Bool
  , filter : String
  }


init : Model
init =
  { sortiere = Lieferdatum
  , sortiereVorwärts = True
  , filter = ""
  }


type Eigenschaft
  = Artikelname
  | Lieferdatum
  | Kundenname


--------------------------------------------------------------------------
------------------------------- UPDATE -----------------------------------

type Msg
  = KlickeEigenschaft Eigenschaft
  | GeheZu            Programmansicht
  | ÄndereFilter      String


update : Msg -> Model -> Either Programmansicht Model
update msg model =
  case msg of
    KlickeEigenschaft eigenschaft -> Right
      { model
      | sortiere = eigenschaft
      , sortiereVorwärts =
          if eigenschaft == model.sortiere
            then not model.sortiereVorwärts
            else True
      }
    GeheZu ansicht -> Left ansicht
    ÄndereFilter neuerFilter -> Right { model | filter = neuerFilter }


------------------------------------------------------------------------
------------------------------- VIEW -----------------------------------


type alias ArtikelInfo =
  { kundenname  : String
  , lieferdatum : Datum.Model
  , artikelname : String
  , bestellId   : Int
  }


view :
  { model       : Model
  , lieferungen : List Lieferung
  }
  -> Elem Msg
view { model , lieferungen } =
  let
    space = spacin 10
    columnSizes = [250, 250, 250]
    sortButton eigenschaft txt =
      let
        arrow =
          if eigenschaft == model.sortiere
            then if model.sortiereVorwärts then " ↓" else " ↑"
            else ""
      in
        button Stil.SortButton [center, onClick (KlickeEigenschaft eigenschaft)]
          (text (txt ++ arrow))
  in
    el Stil.Neutral [spacin 20, center] <| column Stil.Neutral [spacin 10, paddingBottom (vergr 20)] <|
      [ button Stil.ButtonSmall
          [onClick <| GeheZu AnsichtÜbersicht, height (pxx 30)]
          (text "Zur Hauptübersicht <--")
      , viewFilterBox { aktuellerFilter = model.filter }
      , Tab.reihe [space] columnSizes
          [ sortButton Artikelname "Artikelname"
          , sortButton Kundenname  "Kunde"
          , sortButton Lieferdatum "Lieferdatum"
          ]
      ]
      ++ viewArtikelListe
          { artLstUnverarbeitet = List.concatMap artikelinfos lieferungen
          , columnSizes         = columnSizes
          , space               = space
          , sortiere            = model.sortiere
          , sortiereVorwärts    = model.sortiereVorwärts
          , filter              = model.filter
          }


viewFilterBox : { aktuellerFilter : String } -> Elem Msg
viewFilterBox arg =
  Input.search Stil.TextField []
    { onChange = ÄndereFilter
    , value    = arg.aktuellerFilter
    , label    = Input.placeholder
                    { text = "Suchanfrage"
                    , label = Input.hiddenLabel ""
                    }
    , options  = []
    }


artikelinfos : Lieferung -> List ArtikelInfo
artikelinfos lieferung =
  List.map
    (\bestellung ->
      { kundenname  = lieferung.kundenname
      , lieferdatum = lieferung.lieferdatum
      , artikelname = bestellung.artikelbezeichnung
      , bestellId   = lieferung.id
      })
    lieferung.bestellungen


filterArtikelLst : { filter : String, artikelLst : List ArtikelInfo } -> List ArtikelInfo
filterArtikelLst arg =
  List.filter
    (\artikel ->
           istTeilString { ganzerString = artikel.kundenname,  teilString = arg.filter }
        || istTeilString { ganzerString = artikel.artikelname, teilString = arg.filter })
    arg.artikelLst


viewArtikelListe :
  { artLstUnverarbeitet : List ArtikelInfo
  , columnSizes         : List Float
  , space               : Attr Msg
  , sortiere            : Eigenschaft
  , sortiereVorwärts    : Bool
  , filter              : String
  }
  -> List (Elem Msg)
viewArtikelListe { artLstUnverarbeitet, columnSizes, space, sortiere, sortiereVorwärts, filter } =
  let
    artLstGefiltert = filterArtikelLst { artikelLst = artLstUnverarbeitet, filter = filter}
    argsFürArtikel =
      { sortiert = sortiere
      , zeige = Artikelname
      , artikelLst = artikelLst
      }
    artikelLst =
      sortiereArtLst
        { sortiere         = sortiere
        , sortiereVorwärts = sortiereVorwärts
        , artikelLst       = artLstGefiltert
        }
  in
    List.map4
      (\a b c artikel ->
        button Stil.HiddenButton
          [onClick <| GeheZu <| AnsichtDetails artikel.bestellId]
          (Tab.reihe [space] columnSizes [a, b, c]))
      (viewEigenschaft argsFürArtikel)
      (viewEigenschaft { argsFürArtikel | zeige = Kundenname } )
      (viewEigenschaft { argsFürArtikel | zeige = Lieferdatum } )
      artikelLst


sortiereArtLst :
  { sortiere         : Eigenschaft
  , sortiereVorwärts : Bool
  , artikelLst       : List ArtikelInfo
  }
  -> List ArtikelInfo
sortiereArtLst   {sortiere, artikelLst, sortiereVorwärts} =
  let
    sortierteListe = List.sortWith (eigenschaftCompare sortiere) artikelLst
  in
    if sortiereVorwärts
    then sortierteListe
    else List.reverse sortierteListe


eigenschaftCompare : Eigenschaft -> ArtikelInfo -> ArtikelInfo -> Order
eigenschaftCompare eigenschaft =
  case eigenschaft of
    Artikelname -> compare |> on .artikelname
    Lieferdatum ->
      let
        lief a =
          case a.lieferdatum of
            Datum.Datum date -> -(Date.toTime date)
            Datum.DatumStr str -> 0
      in compare |> on lief
    Kundenname -> compare |> on .kundenname


on : (c -> a) -> (a -> a -> b) -> (c -> c -> b)
on   f           bi                a    b    =
  bi (f a) (f b)


viewEigenschaft :
  { sortiert   : Eigenschaft
  , zeige      : Eigenschaft
  , artikelLst : List ArtikelInfo
  }
  -> List (Elem Msg)
viewEigenschaft { sortiert, zeige, artikelLst } =
  case (sortiert, zeige) of
    (Lieferdatum, Lieferdatum) ->
      let lieferdaten = List.map .lieferdatum artikelLst
      in
        List.map2
          (\datum color ->
            el color [] <| text <| Datum.toStr datum)
          lieferdaten
          (DayColors.farbSegmente <|
            List.map
              (\datum ->
                case datum of
                  Datum.Datum date -> Just date
                  Datum.DatumStr str -> Nothing)
              lieferdaten)
    _ ->
      (\artikel ->
        case zeige of
          Lieferdatum -> text <| Datum.toStr artikel.lieferdatum
          Artikelname -> text <| artikel.artikelname
          Kundenname  -> text <| artikel.kundenname )
      |> \view -> List.map view artikelLst


--misc

istTeilString :
  { ganzerString : String
  , teilString   : String
  }
  -> Bool
istTeilString arg =
  Regex.contains
    (Regex.caseInsensitive <| Regex.regex arg.teilString)
    arg.ganzerString
