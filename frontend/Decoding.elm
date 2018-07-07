module Decoding exposing (..)

import Json.Decode as Json
import Types exposing (..)
import Date exposing (Date)

type ServerMsg
  = TestMsg String
  | ServerLöscheBestellung Int Int
  | ServerLöscheLieferung Int
  | ServerChangeLieferung Lieferung
  | ServerZeigeNeueLieferung Int
  | ServerSetzeAllesZurück
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
    [ decodeTestMsg
    , decodeLöscheLieferung
    , decodeLöscheBestellung
    , decodeChangeLieferung
    , decodeZeigeNeueLieferung
    , decodeSetzeAllesZurück
    ]

decodeZeigeNeueLieferung : Json.Decoder ServerMsg
decodeZeigeNeueLieferung =
  Json.map ServerZeigeNeueLieferung Json.int
  |> Json.field "ZeigeNeueLieferung"

decodeTestMsg : Json.Decoder ServerMsg
decodeTestMsg =
  Json.map TestMsg Json.string
  |> Json.field "TestMsg"

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
  -- Json.map (\_ -> ServerSetzeAllesZurück)
  -- |>
  Json.field "SetzeZurück" Json.string
  |> Json.andThen (\_ -> Json.succeed ServerSetzeAllesZurück)

-----------------------------------------
------------ misc --------------------------

decodeLieferung : Json.Decoder Lieferung
decodeLieferung =
  Json.map5 Lieferung
    (Json.field "_bestelldatum" decodeDate)
    (Json.field "_lieferdatum" Json.string)
    (Json.field "_bestellungen" (Json.list decodeBestellung))
    (Json.field "_kundenname" Json.string)
    (Json.field "_lid" Json.int)

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
