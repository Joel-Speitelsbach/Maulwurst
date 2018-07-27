{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import qualified Data.Aeson.TH as Ath
import qualified Data.ByteString.Lazy.Char8 as B

data Lieferung = Lieferung
  { _bestelldatum     :: Double
  , _lieferdatum      :: Either String Double
  , _kundenname       :: String
  , _bestelltyp       :: Bestelltyp
  , _partyserviceData :: PartyserviceData
  , _inPapierkorb     :: Maybe Double
  , _lid              :: Int
  , _bestellungen     :: [Bestellung]
  } deriving (Show, Eq)

data Bestellung = Bestellung
  { _plu                :: String
  , _artikelbezeichnung :: String
  , _menge              :: String
  , _status             :: String
  , _freitext           :: String
  , _bid                :: Int
  } deriving (Show, Eq)

data Bestelltyp
  = Adelsheim
  | Merchingen
  | Partyservice
  deriving (Show, Eq)

data PartyserviceData = PartyserviceData
  { _adresse           :: String
  , _telefon           :: String
  , _veranstaltungsort :: String
  , _personenanzahl    :: String
  } deriving (Show, Eq)

$(Ath.deriveJSON Ath.defaultOptions ''Lieferung)
$(Ath.deriveJSON Ath.defaultOptions ''Bestellung)
$(Ath.deriveJSON Ath.defaultOptions ''Bestelltyp)
$(Ath.deriveJSON Ath.defaultOptions ''PartyserviceData)

makeLenses ''Lieferung
makeLenses ''Bestellung


--------------------------------------------------------------
-------------- receiving stuff ------------------------------

data Recieving
  = UpdateLieferung Lieferung
  | PapierkorbLieferung Int Bool
  -- | LöscheBestellung Int Int
  | NeueLieferung
  | NeueBestellung Int
  | MalformedMsg B.ByteString
  deriving (Show)
