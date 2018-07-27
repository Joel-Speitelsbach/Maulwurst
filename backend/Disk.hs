module Disk
    ( speichereLieferungenPeriodisch
    , öffneLieferungen
    ) where

import Control.Concurrent.STM as STM
import Control.Concurrent (forkIO,threadDelay)
import Types
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson as A
import Control.Monad (forever, when)
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import Data.Maybe (isNothing)

{-
  this module is about file handling
-}

speichereLieferungenPeriodisch lieferungenT =
  do
    letzteLiefT <- newTVarIO =<< atomically (readTVar lieferungenT)
    forkIO $ forever $ do
      lieferungen <- atomically $ do
        lief <- readTVar lieferungenT
        letzteLief <- swapTVar letzteLiefT lief
        STM.check $ lief /= letzteLief
        return lief
      speichereLieferungen lieferungen
      putStrLn "data saved"
      threadDelay (1000 * 1000)

speichereLieferungen :: [Lieferung] -> IO ()
speichereLieferungen lieferungen =
  atomicWriteFile lagerpfad encoded
  where
    encoded = B.unlines $ map A.encode lieferungen

öffneLieferungen :: IO (Maybe [Lieferung])
öffneLieferungen =
  do
  lieferungenJson <- B.lines <$> B.readFile lagerpfad
  let
    mayLieferungen = mapM A.decode lieferungenJson
  when (isNothing mayLieferungen)
    ( putStrLn $ "lagerung ist defekt. prüfe vllt mal die zeilen"
    )
  return mayLieferungen

lagerpfad = "lieferungen.lst"
