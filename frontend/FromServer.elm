module FromServer exposing (..)

import Json.Decode as Json
import CommonTypes exposing (..)
import CommonnTypes exposing (..)
import Date exposing (Date)
import Datum

type ServerMsg
  = ServerLöscheBestellung Int Int
  | ServerLöscheLieferung Int
  | ServerChangeLieferung Lieferung
  | ServerZeigeNeueLieferung Int
  | ServerSetzeAllesZurück
  | ServerHerzschlag
  | MalformedMsg String

----------------------------------------------
------ decode messages ------------------------

parseServerMsg : String -> ServerMsg
parseServerMsg msg =
  Json.decodeString decodeServerMsg msg
  |> Result.withDefault (MalformedMsg msg)

decodeServerMsg : Json.Decoder ServerMsg
decodeServerMsg =
  Json.oneOf
    [ decodeLöscheLieferung
    , decodeLöscheBestellung
    , decodeChangeLieferung
    , decodeZeigeNeueLieferung
    , decodeSetzeAllesZurück
    , decodeHerzschlag
    ]

decodeZeigeNeueLieferung : Json.Decoder ServerMsg
decodeZeigeNeueLieferung =
  Json.map ServerZeigeNeueLieferung Json.int
  |> Json.field "ZeigeNeueLieferung"

decodeLöscheLieferung : Json.Decoder ServerMsg
decodeLöscheLieferung =
  Json.map ServerLöscheLieferung Json.int
  |> Json.field "LöscheLieferung"

decodeLöscheBestellung : Json.Decoder ServerMsg
decodeLöscheBestellung =
  Json.map2 ServerLöscheBestellung
    (Json.field "Lieferung" Json.int)
    (Json.field "LöscheBestellung" Json.int)

decodeChangeLieferung : Json.Decoder ServerMsg
decodeChangeLieferung =
  Json.map ServerChangeLieferung decodeLieferung

decodeSetzeAllesZurück : Json.Decoder ServerMsg
decodeSetzeAllesZurück =
  Json.map (\_ -> ServerSetzeAllesZurück) <|
    Json.field "SetzeZurück" Json.string

decodeHerzschlag : Json.Decoder ServerMsg
decodeHerzschlag =
  Json.map (\_ -> ServerHerzschlag) <|
    Json.field "Bum..." Json.string

-----------------------------------------
------------ misc --------------------------

decodeLieferung : Json.Decoder Lieferung
decodeLieferung =
  Json.map8 Lieferung
    (Json.field "_bestelldatum" decodeDate)
    (Json.field "_lieferdatum" decodeEitherDate)
    (Json.field "_bestellungen" (Json.list decodeBestellung))
    (Json.field "_kundenname" Json.string)
    (Json.field "_bestelltyp" decodeBestelltyp)
    (Json.field "_partyserviceData" decodePartyserviceData)
    (Json.field "_inPapierkorb" <| Json.nullable decodeDate)
    (Json.field "_lid" Json.int)

decodeEitherDate : Json.Decoder Datum.Model
decodeEitherDate =
  Json.oneOf
    [ Json.map Datum.Datum <|
        Json.field "Right" decodeDate
    , Json.map Datum.DatumStr <|
        Json.field "Left"  Json.string
    ]

decodeDate =
  Json.map Date.fromTime Json.float

decodeBestellung : Json.Decoder Bestellung
decodeBestellung =
  Json.map6 Bestellung
    (Json.field "_plu" Json.string)
    (Json.field "_artikelbezeichnung" Json.string)
    (Json.field "_menge" Json.string)
    (Json.field "_status" decodeStatus)
    (Json.field "_freitext" Json.string)
    (Json.field "_bid" Json.int)

decodeBestelltyp : Json.Decoder Bestelltyp
decodeBestelltyp =
  Json.string
  |> Json.andThen
      (\str -> case str of
          "Adelsheim" -> Json.succeed Adelsheim
          "Merchingen" -> Json.succeed Merchingen
          "Partyservice" -> Json.succeed Partyservice
          _ -> Json.fail "invalid Bestelltyp"
      )

decodePartyserviceData : Json.Decoder PartyserviceData
decodePartyserviceData =
  Json.map4 PartyserviceData
    (Json.field "_adresse" Json.string)
    (Json.field "_telefon" Json.string)
    (Json.field "_veranstaltungsort" Json.string)
    (Json.field "_personenanzahl" Json.string)

testJson =
  """
  { "tag": "Partyservice"
  , "_adresse": "Kabl 4567"
  , "_telefon": "9999"
  , "_veranstaltungsort": "Rübelhausen"
  }
  """

decodeStatus : Json.Decoder Status
decodeStatus =
  let
    parse str =
      case str of
        "InBearbeitung" -> InBearbeitung
        "Fertig" -> Fertig
        "Neu" -> Neu
        _ -> Neu
  in
    Json.map parse Json.string
