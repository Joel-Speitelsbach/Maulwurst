module ToServer exposing (..)

import CommonnTypes exposing (..)
import CommonTypes exposing (..)
import Date exposing (Date)
import Datum
import Json.Encode as Json
import Local
import WebSocket


-- Nachrichten an Server

send : Json.Value -> Cmd msg
send msg =
  WebSocket.send Local.serverUrl (Json.encode 0 msg)

löscheBestellung : Int -> Int -> Json.Value
löscheBestellung lid bid =
  Json.object
    [ ("LöscheBestellung" , Json.int bid)
    , ("Lieferung"        , Json.int lid)
    ]

papierkorbLieferung : Int -> Bool -> Json.Value
papierkorbLieferung id bool =
  Json.object
    [ ("PapierkorbLieferung", Json.bool bool)
    , ("Lieferung", Json.int id)
    ]

updateLieferung : Lieferung -> Json.Value
updateLieferung lieferung =
  Json.object
    [ ("UpdateLieferung" , encodeLieferung lieferung)
    ]

neueLieferung : Json.Value
neueLieferung =
  Json.object
    [ ("NeueLieferung", Json.string "")
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
    [ ("_bestelldatum"    , encodeDate lieferung.bestelldatum)
    , ("_lieferdatum"     , encodeEitherDate lieferung.lieferdatum)
    , ("_bestellungen"    , Json.list <| List.map encodeBestellung lieferung.bestellungen)
    , ("_kundenname"      , Json.string lieferung.kundenname)
    , ("_bestelltyp"      , encodeBestelltyp lieferung.bestelltyp)
    , ("_partyserviceData", encodePartyserviceData lieferung.partyserviceData)
    , ( "_inPapierkorb"
      , case lieferung.inPapierkorb of
          Just date -> encodeDate date
          Nothing   -> Json.null
      )
    , ("_lid"         , Json.int lieferung.id)
    ]

encodeBestelltyp : Bestelltyp -> Json.Value
encodeBestelltyp bestelltyp =
    case bestelltyp of
      Adelsheim    -> Json.string "Adelsheim"
      Merchingen   -> Json.string "Merchingen"
      Partyservice -> Json.string "Partyservice"

encodePartyserviceData : PartyserviceData -> Json.Value
encodePartyserviceData party =
  Json.object
    [ ("_adresse",           Json.string party.adresse)
    , ("_telefon",           Json.string party.telefon)
    , ("_veranstaltungsort", Json.string party.veranstaltungsort)
    , ("_personenanzahl",    Json.string party.personenanzahl)
    ]

encodeEitherDate : Datum.Model -> Json.Value
encodeEitherDate either =
  Json.object
    [ case either of
        Datum.Datum date -> ("Right", encodeDate date)
        Datum.DatumStr str -> ("Left", Json.string str)
    ]

encodeDate : Date -> Json.Value
encodeDate jetzt = Json.float (Date.toTime jetzt)
