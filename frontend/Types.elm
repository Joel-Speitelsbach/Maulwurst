module Types exposing (..)

import Date exposing (Date)
import Datum
import Time exposing (Time)
import DatePicker exposing (DatePicker)
import CommonTypes exposing (..)

type alias Model =
  { lieferungen : List Lieferung
  , übersichtZustand : ÜbersichtZustand
  , ansicht : Ansicht
  , letzteServerNachricht : Time
  , jetzt : Time
  }

type alias Lieferung =
  { bestelldatum : Date
  , lieferdatum : Datum.Model
  , bestellungen : List Bestellung
  , kundenname : String
  , bestelltyp : Bestelltyp
  , partyserviceData : PartyserviceData
  , inPapierkorb : Maybe Date
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

type alias PartyserviceData =
  { adresse : String
  , telefon : String
  , veranstaltungsort : String
  , personenanzahl : String
  }

type Ansicht
  = Übersicht
  | Details DetailsAnsicht

type alias DetailsAnsicht =
  { id : Int
  , modus : DetailsModus
  }

type DetailsModus
  = DetailsNormal
  | Reloading
  | LöschDialog

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

--------------------------------------------------------------------
------------------------ common values ---------------------------

statusString status =
  case status of
    Just Neu -> "Neu"
    Just InBearbeitung -> "Bearbeitung"
    Just Fertig -> "Fertig"
    Nothing -> "Papierkorb"

bestelltypString bestelltyp =
  case bestelltyp of
    Adelsheim -> "Adelsheim"
    Merchingen -> "Merchingen"
    Partyservice -> "Partyservice"

inPapierkorbBool lieferung =
  Maybe.map (\_ -> True) lieferung.inPapierkorb
  |> Maybe.withDefault False

serverUrl = "ws://192.168.178.45:18539"
-- serverUrl = "ws://37.221.194.181:18539"
