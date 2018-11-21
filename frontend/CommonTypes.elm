module CommonTypes exposing (..)

import Element exposing (..)
import Stil exposing (Stil)
import CommonnTypes exposing (..)
import Date exposing (Date)
import Datum


type alias Lieferung =
  { bestelldatum     : Date
  , lieferdatum      : Datum.Model
  , bestellungen     : List Bestellung
  , kundenname       : String
  , bestelltyp       : Bestelltyp
  , partyserviceData : PartyserviceData
  , inPapierkorb     : Maybe Date
  , id               : Int
  }


type alias Bestellung =
  { plu                : String
  , artikelbezeichnung : String
  , menge              : String
  , status             : Status
  , freitext           : String
  , id                 : Int
  }


type alias PartyserviceData =
  { adresse           : String
  , telefon           : String
  , veranstaltungsort : String
  , personenanzahl    : String
  , lieferservice     : Bool
  , mitChafingDish    : Bool
  }


type Programmansicht
  = AnsichtDetails Int
  | AnsichtÜbersicht
  | AnsichtArtikelübersicht


type alias Elem msg = Element Stil Never msg
type alias Attr msg = Attribute Never msg
type alias Attrs msg = List (Attr msg)


--------------------------------------------------------------------
------------------------ common values ---------------------------


statusString status =
  case status of
    Just Neu           -> "Neu"
    Just InBearbeitung -> "Bearbeitung"
    Just Fertig        -> "Fertig"
    Nothing            -> "Papierkorb"


inPapierkorbBool lieferung =
  Maybe.map (\_ -> True) lieferung.inPapierkorb
  |> Maybe.withDefault False


bestelltypString bestelltyp =
  case bestelltyp of
    Adelsheim    -> "Adelsheim"
    Merchingen   -> "Merchingen"
    Partyservice -> "Partyservice"


ixLieferung lieferungen lieferungsId =
  List.filter (\l -> l.id == lieferungsId) lieferungen
  |> List.head
