module Types exposing (..)

import Date exposing (Date)

type alias Lieferung =
  { bestelldatum : Date
  , lieferdatum : String
  , bestellungen : List Bestellung
  , kundenname : String
  , id : Int
  }

type alias Bestellung =
  { plu : String
  , artikelbezeichnung : String
  , menge : String
  , status : Status
  , freitext : String
  , id : Int
  }

type Status
  = Neu
  | InBearbeitung
  | Fertig
