{-# LANGUAGE ScopedTypeVariables #-}

module Test.RandomData where

import Types
import Control.Monad.Random as R
import Control.Exception.Base (assert)

import Misc
import qualified Disk


main :: IO ()
main = do
  lieferungen <- genLieferungen
  Disk.speichereLieferungen lieferungen


genLieferungen :: IO [Lieferung]
genLieferungen = do
  now <- getEpochMillisecs
  evalRandIO $ mapM (rLieferung now) [1..50]


rLieferung :: MonadRandom m => Double  -> Int -> m Lieferung
rLieferung                     dateNow    lid    =
  do
    dateBestell <- (-) dateNow     <$> rTimeOffsetBestell
    dateLiefer  <- (+) dateBestell <$> rTimeOffsetLiefer
    Lieferung
      <$> pure dateBestell                            -- _bestelldatum     :: Double
      <*> (pure $ Right dateLiefer)                   -- _lieferdatum      :: Either String Double
      <*> rPersonName                                 -- _kundenname       :: String
      <*> oneOf [Adelsheim, Merchingen, Partyservice] -- _bestelltyp       :: Bestelltyp
      <*> rPartyserviceData                           -- _partyserviceData :: PartyserviceData
      <*> rInPapierkorb dateBestell                   -- _inPapierkorb     :: Maybe Double
      <*> pure lid                                    -- _lid              :: Int
      <*> (mapM rBestellung =<< rBids)                -- _bestellungen     :: [Bestellung]
  where
    rBids :: MonadRandom m => m [Int]
    rBids = do
      nBestellungen <- getRandomR (1,5)
      return [1..nBestellungen]
    rInPapierkorb :: MonadRandom m => Double      -> m (Maybe Double)
    rInPapierkorb                     dateBestell    = do
      timeOffset <- getRandomR
                      ( 1000 * 10
                      , 1000 * 60 * 60 * 24 * 3 )
      let papDate = min dateNow $ dateBestell + timeOffset
      choose 0.9
             (pure Nothing)
             (pure $ Just papDate)


rPartyserviceData :: MonadRandom m => m PartyserviceData
rPartyserviceData                     =
  PartyserviceData
    <$> rText (2,4)                    -- _adresse           :: String
    <*> rNumStr (100000, 999999999999) -- _telefon           :: String
    <*> rName (9, 15)                  -- _veranstaltungsort :: String
    <*> rNumStr (5,30)                 -- _personenanzahl    :: String


rTimeOffsetLiefer :: MonadRandom m => m Double
rTimeOffsetLiefer = getRandomR
                ( 1000 * 60 * 60 * 24 * 14
                , 1000 * 60 * 60 * 24 * 17 )


rTimeOffsetBestell :: MonadRandom m => m Double
rTimeOffsetBestell = getRandomR
                ( 1000 * 10
                , 1000 * 60 * 60 * 24 * 3 )


rBestellung :: MonadRandom m => Int -> m Bestellung
rBestellung                     bid    =
  Bestellung
    <$> rNumStr (1,99) -- _plu
    <*> rName (5,14)   -- _artikelbezeichnung
    <*> rMenge         -- _menge
    <*> rStatus        -- _status
    <*> rText (3,7)   -- _freitext
    <*> pure bid       -- _bid


rNumStr :: MonadRandom m => (Int,Int) -> m String
rNumStr                     range        =
  show <$> getRandomR range


rText :: MonadRandom m => (Int,Int)   -> m String
rText                     nWordsRange    =
  do
    nWords <- getRandomR nWordsRange
    unwords <$> replicateM nWords (rWord (2,12))


rStatus :: MonadRandom m => m String
rStatus = oneOf ["Neu", "InBearbeitung", "Fertig"]


rMenge :: MonadRandom m => m String
rMenge = (++ "g") <$>
         (show <$> ((100 *) <$> getRandomR (1,6 :: Int)))


rArtikel :: MonadRandom m => m String
rArtikel = rName (5,14)


rCharLower :: MonadRandom m => m Char
rCharLower = toEnum <$> getRandomR (fromEnum 'a', fromEnum 'z')


rCharUpper :: MonadRandom m => m Char
rCharUpper = toEnum <$> getRandomR (fromEnum 'A', fromEnum 'Z')


rChar :: MonadRandom m => m Char
rChar = chooseEq rCharLower rCharUpper


rName :: MonadRandom m => (Int, Int) -> m String
rName                     =
  rWordFirst rCharUpper


rPersonName :: MonadRandom m => m String
rPersonName                   =
  fmap unwords $ replicateM 2 $ rName (4,10)


rWord :: MonadRandom m => (Int, Int) -> m String
rWord                     =
  rWordFirst rChar


rWordFirst :: MonadRandom m => m Char -> (Int,  Int)  -> m String
rWordFirst                     first     (lMin, lMax)    = do
  len <- getRandomR (lMin,lMax)
  (:) <$> first <*> replicateM (len-1) rCharLower



--misc


oneOf :: MonadRandom m => [a] -> m a
oneOf                     lst           =
    assert (len > 0) $
      (!!) lst <$> getRandomR (0, len-1)
  where
    len = length lst


chooseEq :: MonadRandom m => m a -> m a -> m a
chooseEq = choose 0.5


choose :: MonadRandom m => Float -> m a  -> m a   -> m a
choose                     prob     first   second   =
  if_ <$> dice prob <*> first <*> second


dice :: MonadRandom m => Float -> m Bool
dice                     p        =
  (>=) p <$> getRandomR (0,1.0)


if_ :: Bool -> a -> a -> a
if_    bool    x    y    =
  if bool then x else y
