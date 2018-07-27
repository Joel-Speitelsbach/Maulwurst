{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Decoding
    ( decodeRecieving
    ) where

import Types
import qualified Data.Maybe as M
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as At
import qualified Data.ByteString.Lazy.Char8 as B

type Decoder a = A.Object -> At.Parser a

decodeRecieving :: B.ByteString -> Recieving
decodeRecieving msg =
  maybe (MalformedMsg msg) id $
    M.listToMaybe . M.catMaybes . maybe [] id $ do
      obj <- A.decode msg
      return $
        map (flip At.parseMaybe obj)
          [ decodeUpdateLieferung
          -- , decodeLöscheBestellung
          , decodePapierkorbLieferung
          , decodeNeueBestellung
          , decodeNeueLieferung
          ]

decodeUpdateLieferung :: Decoder Recieving
decodeUpdateLieferung obj =
  do
    lieferung <- obj .: "UpdateLieferung"
    return $ UpdateLieferung lieferung

-- decodeLöscheBestellung :: Decoder Recieving
-- decodeLöscheBestellung obj =
--   do
--     bid <- obj .: "LöscheBestellung"
--     lid <- obj .: "Lieferung"
--     return $ LöscheBestellung lid bid

decodePapierkorbLieferung :: Decoder Recieving
decodePapierkorbLieferung obj =
  do
    bool <- obj .: "PapierkorbLieferung"
    lid  <- obj .: "Lieferung"
    return $ PapierkorbLieferung lid bool

decodeNeueBestellung :: Decoder Recieving
decodeNeueBestellung obj =
  do
    lid <- obj .: "NeueBestellung"
    return $ NeueBestellung lid

decodeNeueLieferung :: Decoder Recieving
decodeNeueLieferung obj =
  do
    (_ :: String) <- obj .: "NeueLieferung"
    return $ NeueLieferung
