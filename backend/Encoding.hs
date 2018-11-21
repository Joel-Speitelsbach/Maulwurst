{-# LANGUAGE OverloadedStrings #-}

module Encoding where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy.Char8 as B

löscheBestellung :: Int -> Int -> B.ByteString
löscheBestellung lid bid =
  A.encode (LöscheBestellungMsg lid bid)
data LöscheBestellungMsg = LöscheBestellungMsg Int Int
instance A.ToJSON LöscheBestellungMsg where
  toJSON (LöscheBestellungMsg lid bid) =
    A.object ["LöscheBestellung" .= bid
             ,"Lieferung"        .= lid]

löscheLieferung :: Int -> B.ByteString
löscheLieferung lid =
  A.encode (LöscheLieferungMsg lid)
newtype LöscheLieferungMsg = LöscheLieferungMsg Int
instance A.ToJSON LöscheLieferungMsg where
  toJSON (LöscheLieferungMsg lid) =
    A.object ["LöscheLieferung" .= lid]

zeigeNeueLieferung :: Int -> B.ByteString
zeigeNeueLieferung lid =
  A.encode (ZeigeNeueLieferung lid)
newtype ZeigeNeueLieferung = ZeigeNeueLieferung Int
instance A.ToJSON ZeigeNeueLieferung where
  toJSON (ZeigeNeueLieferung lid) =
    A.object ["ZeigeNeueLieferung" .= lid]

setzeZurück :: B.ByteString
setzeZurück =
  A.encode SetzeZurück
data SetzeZurück = SetzeZurück
instance A.ToJSON SetzeZurück where
  toJSON SetzeZurück =
    A.object ["SetzeZurück" .= ("alles" :: String)]

herzschlag :: B.ByteString
herzschlag =
  A.encode Herzschlag
data Herzschlag = Herzschlag
instance A.ToJSON Herzschlag where
  toJSON Herzschlag =
    A.object ["Bum..." .= ("" :: String)]
