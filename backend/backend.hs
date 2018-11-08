{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- connection imports
import Network.WebSockets as Websocket
import Control.Concurrent (forkIO,threadDelay)
import Control.Monad (forever, void, forM_)
import qualified System.Exit as Exit
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
-- import qualified Network.Wai.Handler.WebSockets as WaiWebsocket
import Network.HTTP.Types (status200)

-- other imports
import Control.Concurrent.STM
import qualified Debug
import qualified Disk
import qualified ListenClients as OnClients
import qualified ManageState as State
import qualified CleanUp
import qualified Encoding as Enc
import qualified Data.Local as Local

------------------------------------------------------------------------------
------------------ connection stuff -------------------------------------------

sendingToClient fromBroadcast connection =
  forever $ do
    msgs <- atomically $ readTChan fromBroadcast
    forM_ msgs $ Websocket.sendTextData connection

recievingFromClient lieferungenT broadcastChannel connection = forever $ do
  msg <- Websocket.receiveData connection
  OnClients.reactRecieving lieferungenT broadcastChannel msg connection

wsApp lieferungenT broadcastChannel pendingConnection = do
  Debug.logNewConnection
  connection <- acceptRequest pendingConnection
  fromBroadcast <- State.initClient connection lieferungenT broadcastChannel
  forkIO $ recievingFromClient lieferungenT broadcastChannel connection
  forkIO $ sendingToClient fromBroadcast connection
  forever $ do
    threadDelay $ 10^6
    Websocket.sendTextData connection $ Enc.herzschlag

-- waiApp :: .. -> Application
-- waiApp wsApp = WaiWebsocket.websocketsOr defaultConnectionOptions wsApp sendHtmlApp

sendHtmlApp :: Application
sendHtmlApp _ respond = respond sendIndex

sendIndex :: Wai.Response
sendIndex = responseFile
    status200
    [("Content-Type", "text/html")]
    "../frontend/index.html"
    Nothing

---------------------------------------------------
-------------------- main ----------------------

main = do
  mayLief <- Disk.öffneLieferungen
  flip (maybe $ Exit.exitFailure) mayLief $ \lief -> do
    void . forkIO $ do
        lieferungenT <- newTVarIO lief
        broadcastChannel <- newBroadcastTChanIO
        Disk.speichereLieferungenPeriodisch lieferungenT
        CleanUp.räumeDatenAufPeriodisch lieferungenT broadcastChannel
        forkIO $ Websocket.runServer
          Local.serverIP
          Local.websocketPort
          (wsApp lieferungenT broadcastChannel)
        Warp.run (Local.websocketPort+1) sendHtmlApp
    void $ getLine
    Exit.exitSuccess
