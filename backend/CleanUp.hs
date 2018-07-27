module CleanUp
    ( räumeDatenAufPeriodisch
    ) where

import Types
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Misc
import Control.Concurrent.STM
import Control.Concurrent (forkIO,threadDelay)
import Control.Monad (forever, void, guard)
import Data.Maybe (isNothing)
import qualified ManageState as State

räumeDatenAufPeriodisch :: TVar [Lieferung] -> TChan [B.ByteString] -> IO ()
räumeDatenAufPeriodisch lieferungenT broadcastChannel =
  void $ forkIO $ forever $ do
    räumeDatenAuf lieferungenT broadcastChannel
    waitSomeTime
  where
  waitSomeTime = threadWarte $ (/10) $ min zeitVorLöschung zeitVorPapierkorb

räumeDatenAuf lieferungenT broadcastChannel =
  do
    jetzt <- Misc.getEpochMillisecs
    atomically $ leerePapierkorb jetzt lieferungenT broadcastChannel
    atomically $ füllePapierkorb jetzt lieferungenT broadcastChannel

leerePapierkorb jetzt lieferungenT broadcastChannel =
  State.löscheLieferungen alt lieferungenT broadcastChannel
  where
  alt lieferung =
    maybe False
      (> zeitVorLöschung)
      maybeAlter
    where
    maybeAlter = (jetzt -) <$> papZeit lieferung

füllePapierkorb jetzt lieferungenT broadcastChannel =
  State.aktualisiereLieferungen maybeStecke lieferungenT broadcastChannel
  where
  maybeStecke lieferung =
    maybe Nothing
      (\alter ->
          if alter < zeitVorPapierkorb
          then Nothing
          else Just lieferung { _inPapierkorb = Just jetzt }
      )
      maybeAlter
    where
    maybeAlter = (jetzt -) <$> aktivDatum lieferung

aktivDatum :: Lieferung -> Maybe Double
aktivDatum Lieferung{_inPapierkorb = pap
                    , _lieferdatum = eitherLiefer
                    , _bestelldatum = bestell
                    } = do
  guard $ isNothing pap
  case eitherLiefer of
    Right liefer -> return liefer
    Left _ -> return bestell

papZeit :: Lieferung -> Maybe Double
papZeit = _inPapierkorb

secs = (* 1000)
mins = (*60) . secs
hours = (*60) . mins
days = (*24) . hours

zeitVorPapierkorb :: Double
zeitVorPapierkorb = days 7
zeitVorLöschung = days 7

threadWarte :: Double -> IO ()
threadWarte = threadDelay . (*1000) . floor
