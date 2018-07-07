module Encoding exposing (..)

import Json.Encode as Json
import Types exposing (..)
import Date exposing (Date)


-- Nachrichten an Server

löscheBestellung : Int -> Int -> Json.Value
löscheBestellung lid bid =
  Json.object
    [ ("LöscheBestellung" , Json.int bid)
    , ("Lieferung"        , Json.int lid)
    ]

löscheLieferung : Int -> Json.Value
löscheLieferung id =
  Json.object
    [ ("LöscheLieferung", Json.int id)
    ]

updateLieferung : Lieferung -> Json.Value
updateLieferung lieferung =
  Json.object
    [ ("UpdateLieferung" , encodeLieferung lieferung)
    ]

neueLieferung : Date -> Json.Value
neueLieferung jetzt =
  Json.object
    [ ("NeueLieferung", encodeDate jetzt)
    ]

neueBestellung : Int -> Json.Value
neueBestellung lid =
  Json.object
    [ ("NeueBestellung", Json.int lid)
    ]

encodeDate : Date -> Json.Value
encodeDate jetzt = Json.float (Date.toTime jetzt)

-- Misc

encodeBestellung : Bestellung -> Json.Value
encodeBestellung bestellung =
  Json.object
    [ ("_plu"               , Json.string bestellung.plu)
    , ("_artikelbezeichnung", Json.string bestellung.artikelbezeichnung)
    , ("_menge"             , Json.string bestellung.menge)
    , ("_status"            , Json.string (toString bestellung.status))
    , ("_freitext"          , Json.string bestellung.freitext)
    , ("_bid"               , Json.int bestellung.id)
    ]

encodeLieferung : Lieferung -> Json.Value
encodeLieferung lieferung =
  Json.object
    [ ("_bestelldatum", encodeDate lieferung.bestelldatum)
    , ("_lieferdatum" , Json.string lieferung.lieferdatum)
    , ("_bestellungen", Json.list <| List.map encodeBestellung lieferung.bestellungen)
    , ("_kundenname"  , Json.string lieferung.kundenname)
    , ("_lid"         , Json.int lieferung.id)
    ]

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
