{-# LANGUAGE OverloadedStrings #-}
import Network.Connection
  (TLSSettings(TLSSettingsSimple),TLSSettings(..), ConnectionParams(..)
  ,initConnectionContext,connectTo,connectionGetChunk,connectionPut)
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy.Char8 as B
import Network.WebSockets
  (ClientApp, receiveData, sendClose, sendTextData, defaultConnectionOptions
  ,runClientWithStream,runClient)
import Network.WebSockets.Stream (makeStream)
import Data.ByteString.Lazy (toStrict)

main :: IO ()
-- main = runSecureWebSocketClient ws
main = runClient "localhost" 3000 "/" ws
-- main = runSecureClient "echo.websocket.org" 443 "/" ws

runSecureWebSocketClient app = do
  let host = "localhost"
  let port = 3000
  let path = "/"
  let options = defaultConnectionOptions
  let headers = []
  let tlsSettings = TLSSettingsSimple
        -- This is the important setting.
        { settingDisableCertificateValidation = True
        , settingDisableSession = False
        , settingUseServerName = False
        }
  let connectionParams = ConnectionParams
        { connectionHostname = host
        , connectionPort = port
        , connectionUseSecure = Just tlsSettings
        , connectionUseSocks = Nothing
        }

  context <- initConnectionContext
  connection <- connectTo context connectionParams
  stream <- makeStream
      (fmap Just (connectionGetChunk connection))
      (maybe (return ()) (connectionPut connection . toStrict))
  runClientWithStream stream host path options headers app

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"

    void . forkIO . forever $ do
        message <- receiveData connection
        B.putStrLn $ "from Server: " `B.append` message

    let loop = do
            line <- getLine
            unless (null line) $ do
                sendTextData connection (pack line)
                loop
    loop

    sendClose connection (pack "Bye!")
