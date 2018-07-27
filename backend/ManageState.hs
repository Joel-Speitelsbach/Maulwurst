module ManageState where

import qualified Data.ByteString.Lazy.Char8 as B
import Control.Concurrent.STM as STM
import qualified Encoding as Enc
import Types
import qualified Data.Aeson as A
import Network.WebSockets as Websocket
import Data.Maybe (catMaybes)
import Control.Lens (over)
import Data.List (partition)

type Task inp = TaskR inp ()
type TaskR inp out = inp -> TVar [Lieferung] -> TChan [B.ByteString] -> STM out
type TaskM m inp out = inp -> TVar [Lieferung] -> TChan [B.ByteString] -> m out

------------------------------------------------------------------------
-------------------------- core tasks ----------------------------------

aktualisiereLieferungen :: Task (Lieferung -> Maybe Lieferung)
aktualisiereLieferungen mapper = aktualisiereLieferungenOptionalCleanup (mapper, True)

aktualisiereLieferungenOptionalCleanup :: Task (Lieferung -> Maybe Lieferung, Bool)
aktualisiereLieferungenOptionalCleanup (mapper, withCleanup) lieferungenT broadcastChannel =
  do
    alteLieferungen <- readTVar lieferungenT
    let
      (neueLieferungen, msgss) = unzip $ map extMapper alteLieferungen
      extMapper lief =
        maybe (lief, [])
          (\newLief ->
              let (geputzteLief, delMsgs) = cleanUpLieferung newLief
                  msgs = A.encode geputzteLief : delMsgs
              in
                if withCleanup
                  then (geputzteLief, msgs)
                  else (newLief, [A.encode newLief])
          )
          $ mapper lief
    writeTVar lieferungenT neueLieferungen
    writeTChan broadcastChannel $ concat msgss

löscheLieferungen :: Task (Lieferung -> Bool)
löscheLieferungen prec lieferungenT broadcastChannel =
  do
    lieferungen <- readTVar lieferungenT
    let
      (toDel, neueLieferungen) = partition prec lieferungen
      msgs = map (Enc.löscheLieferung . _lid) toDel
    writeTVar lieferungenT neueLieferungen
    writeTChan broadcastChannel $ msgs

fügeNeueLieferungHinzu :: TaskR Double Int
fügeNeueLieferungHinzu millisecs lieferungenT broadcastChannel =
  do
    lieferungen <- readTVar lieferungenT
    let
      newID = (+1) $ maximum $ (0:) $ map _lid lieferungen
      pdata = PartyserviceData "" "" "" ""
      neueLieferung = Lieferung millisecs (Left "") "" Merchingen pdata Nothing newID []
    writeTVar lieferungenT (neueLieferung : lieferungen)
    writeTChan broadcastChannel $ [A.encode neueLieferung]
    return newID

deleteBestellung :: Task (Int,Int)
deleteBestellung (lid_del, bid_del) lieferungenT broadcastChannel =
  do
    let überLieferung =
          over bestellungen $ filter $ (bid_del /=) . _bid
        msg = Enc.löscheBestellung lid_del bid_del
    modifyTVar lieferungenT $ map überLieferung
    writeTChan broadcastChannel [msg]

----------------------------------------------------------------
------------------ specific tasks  ----------------------------

updateLieferung :: Task Lieferung
updateLieferung lieferung = aktualisiereLieferungen maybeReplace where
  maybeReplace lief | _lid lief == _lid lieferung  = Just lieferung
                    | otherwise                    = Nothing

fügeNeueBestellungHinzu :: Task Int
fügeNeueBestellungHinzu liefId = aktualisiereLieferungenOptionalCleanup (maybeReplace, False) where
  maybeReplace lief | _lid lief == liefId  = Just neueLief
                    | otherwise            = Nothing
    where
      newID = (+1) $ maximum $ (0:) $ map _bid $ _bestellungen lief
      neueLief = over bestellungen (++ [neueBestellung]) lief
      neueBestellung = Bestellung "" "" "" "" "" newID

------------------------------------------------------------
----------------------- misc tasks  ------------------------

initClient :: TaskM IO Websocket.Connection (TChan [B.ByteString])
initClient connection lieferungenT broadcastChannel =
  do
    (fromBroadcast, lieferungen) <- atomically $
       (,) <$> dupTChan broadcastChannel
           <*> readTVar lieferungenT
    let initMsgs = Enc.setzeZurück : map A.encode lieferungen
    mapM_ (Websocket.sendTextData connection) initMsgs
    return fromBroadcast

----------------------------------------------------
---------------- helper functions -----------------

emptyBestellung :: Bestellung -> Bool
emptyBestellung bestellung =
  and $ map ( (== "") . ($ bestellung) )
    [ _plu
    , _artikelbezeichnung
    , _menge
    , _freitext
    ]

cleanUpLieferung :: Lieferung -> (Lieferung, [B.ByteString])
cleanUpLieferung lieferung =
  ( over bestellungen (filter $ not . emptyBestellung) lieferung
  , catMaybes $
      flip map (_bestellungen lieferung) $ \bestellung ->
        if emptyBestellung bestellung
        then Just $ Enc.löscheBestellung (_lid lieferung) (_bid bestellung)
        else Nothing
  )
