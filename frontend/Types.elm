module Types exposing (..)

import Date exposing (Date)
import Details
import Time exposing (Time)
import CommonTypes exposing (..)
import CommonnTypes exposing (..)

type alias Model =
  { lieferungen : List Lieferung
  , übersichtZustand : ÜbersichtZustand
  , ansicht : Ansicht
  , jetztM : Maybe Time
  , letzteServerNachricht : Time
  }

type Ansicht
  = Übersicht
  | Details Details.Ansicht

type alias ÜbersichtZustand =
  { neueLieferungAngefordert : Bool
  , sortby : Sortby
  , anzuzeigendeBtypen : List Bestelltyp
  , zeigePapierkorb : Bool
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
