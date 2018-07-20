module ToServer exposing (..)

import Json.Encode as Json
import Types exposing (..)
import Date exposing (Date)
import WebSocket


-- Nachrichten an Server

send : Json.Value -> Cmd msg
send msg =
  WebSocket.send serverUrl (Json.encode 0 msg)

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
    , ("_bestelltyp"  , encodeBestelltyp lieferung.bestelltyp)
    , ("_lid"         , Json.int lieferung.id)
    ]

encodeBestelltyp : Bestelltyp -> Json.Value
encodeBestelltyp bestelltyp =
  Json.object <|
    case bestelltyp of
      Adelsheim  -> [ ("tag", Json.string "Adelsheim") ]
      Merchingen -> [ ("tag", Json.string "Merchingen") ]
      Partyservice party -> encodePartyservice party

encodePartyservice : PartyserviceData -> List (String, Json.Value)
encodePartyservice party =
  [ ("tag", Json.string "Partyservice")
  , ("_adresse", Json.string party.adresse)
  , ("_telefon", Json.string party.telefon)
  , ("_veranstaltungsort", Json.string party.veranstaltungsort)
  , ("_personenanzahl", Json.int party.personenanzahl)
  ]

encodeDate : Date -> Json.Value
encodeDate jetzt = Json.float (Date.toTime jetzt)