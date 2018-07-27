{-# LANGUAGE OverloadedStrings #-}

module ListenClients
    ( reactRecieving
    ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Control.Concurrent.STM as STM
import qualified Encoding as Enc
import Types
import qualified Misc
import Network.WebSockets as Websocket
import Data.Monoid ((<>))
import qualified Decoding as Dec (decodeRecieving)
import qualified ManageState as State

---------------------------------------------------------------
--------------------- react receiving ------------------------

reactRecieving
  :: TVar [Lieferung]
  -> TChan [B.ByteString]
  -> B.ByteString
  -> Websocket.Connection
  -> IO ()
reactRecieving lieferungenT broadcastChannel msg connection =
  case Dec.decodeRecieving msg of
    NeueLieferung -> do
      millisecs <- Misc.getEpochMillisecs
      lid <- atomically $ State.fügeNeueLieferungHinzu millisecs lieferungenT broadcastChannel
      Websocket.sendTextData connection (Enc.zeigeNeueLieferung lid)
    UpdateLieferung lieferung -> atomically $ State.updateLieferung lieferung lieferungenT broadcastChannel
    PapierkorbLieferung lid_pap bool -> do
      pap <- do
        if bool
          then Just <$> Misc.getEpochMillisecs
          else return Nothing
      let maybeReplace lief
            | _lid lief == lid_pap = Just $ lief { _inPapierkorb = pap}
            | otherwise            = Nothing
      atomically $ State.aktualisiereLieferungen maybeReplace lieferungenT broadcastChannel
    NeueBestellung lid -> atomically $ State.fügeNeueBestellungHinzu lid lieferungenT broadcastChannel
    MalformedMsg msg -> do
      B.putStrLn $ "recieving malformed msg: " <> msg
