{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}

-- connection imports
import Network.WebSockets as Websocket
import Control.Concurrent (forkIO,threadDelay)
import Control.Monad as Monad
import qualified Data.Text.Lazy as Text
import Data.IORef
import qualified System.Exit as Exit
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWebsocket
import Network.HTTP.Types (status200)

-- other imports
import qualified Data.Aeson.TH as Ath
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Aeson as A
import Data.Aeson ((.:),(.=))
import qualified Data.Aeson.Types as At
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import qualified Data.Maybe as M
import Data.Function ((&))
import Data.List (partition)
import Control.Lens hiding ((.=))
import Data.Maybe (listToMaybe,isNothing)
import qualified Data.Char as Char
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import System.IO.Unsafe (unsafePerformIO)

data Lieferung = Lieferung
  { _bestelldatum :: Double
  , _lieferdatum  :: String
  , _kundenname   :: String
  , _lid          :: Int
  , _bestellungen :: [Bestellung]
  } deriving (Show, Eq)

data Bestellung = Bestellung
  { _plu                :: String
  , _artikelbezeichnung :: String
  , _menge              :: String
  , _status             :: String
  , _freitext           :: String
  , _bid                :: Int
  } deriving (Show, Eq)

$(Ath.deriveJSON Ath.defaultOptions ''Lieferung)
$(Ath.deriveJSON Ath.defaultOptions ''Bestellung)
makeLenses ''Lieferung
makeLenses ''Bestellung

---------------------------------------------------------------
--------------- sending stuff -------------------------------

changeLieferung :: TVar [Lieferung] -> Lieferung -> STM B.ByteString
changeLieferung lieferungenT lieferung =
  do
    lieferungen <- readTVar lieferungenT
    let
      veränderteLieferungen =
        lieferung : filter ((_lid lieferung /=) . _lid) lieferungen
    writeTVar lieferungenT veränderteLieferungen
    return $ A.encode lieferung

fügeNeueLieferungHinzu :: TVar [Lieferung] -> Double -> STM (B.ByteString, Int)
fügeNeueLieferungHinzu lieferungenT time =
  do
    lieferungen <- readTVar lieferungenT
    let
      newID = (+1) $ maximum $ (0:) $ map _lid lieferungen
      neueLieferung = Lieferung time "" "" newID []
    writeTVar lieferungenT (neueLieferung : lieferungen)
    return $ (A.encode neueLieferung, newID)

fügeNeueBestellungHinzu :: TVar [Lieferung] -> Int -> STM (Maybe B.ByteString)
fügeNeueBestellungHinzu lieferungenT lid_focus =
  do
    lieferungen <- readTVar lieferungenT
    let
      verändert = map addBestellung focus
      newID = (+1) $ maximum $ (0:) $ map _bid $
        concatMap _bestellungen lieferungen
      (focus,rest) = partition ((lid_focus ==) . _lid) lieferungen
      addBestellung = over bestellungen (++ [neueBestellung])
      neueBestellung = Bestellung "" "" "" "" "" newID
    writeTVar lieferungenT (verändert ++ rest)
    return $ do
      lieferung <- listToMaybe verändert
      return $ A.encode lieferung

löscheBestellungMsg :: Int -> Int -> B.ByteString
löscheBestellungMsg lid bid =
  A.encode (LöscheBestellungMsg lid bid)
data LöscheBestellungMsg = LöscheBestellungMsg Int Int
instance A.ToJSON LöscheBestellungMsg where
  toJSON (LöscheBestellungMsg lid bid) =
    A.object ["LöscheBestellung" .= bid
             ,"Lieferung"        .= lid]

löscheLieferungMsg :: Int -> B.ByteString
löscheLieferungMsg lid =
  A.encode (LöscheLieferungMsg lid)
newtype LöscheLieferungMsg = LöscheLieferungMsg Int
instance A.ToJSON LöscheLieferungMsg where
  toJSON (LöscheLieferungMsg lid) =
    A.object ["LöscheLieferung" .= lid]

zeigeNeueLieferungMsg :: Int -> B.ByteString
zeigeNeueLieferungMsg lid =
  A.encode (ZeigeNeueLieferung lid)
newtype ZeigeNeueLieferung = ZeigeNeueLieferung Int
instance A.ToJSON ZeigeNeueLieferung where
  toJSON (ZeigeNeueLieferung lid) =
    A.object ["ZeigeNeueLieferung" .= lid]

setzeZurückMsg :: B.ByteString
setzeZurückMsg =
  A.encode SetzeZurück
data SetzeZurück = SetzeZurück
instance A.ToJSON SetzeZurück where
  toJSON SetzeZurück =
    A.object ["SetzeZurück" .= ("alles" :: String)]

initMsgsT :: TVar [Lieferung] -> STM [B.ByteString]
initMsgsT lieferungenT = do
  lieferungen <- readTVar lieferungenT
  return $
    setzeZurückMsg
    : map A.encode lieferungen

--------------------------------------------------------------
-------------- receiving stuff ------------------------------

data Recieving
  = UpdateLieferung Lieferung
  | LöscheLieferung Int
  | LöscheBestellung Int Int
  | NeueLieferung Double
  | NeueBestellung Int
  | MalformedMsg
  deriving (Show)

reactRecieving
  :: TVar [Lieferung] -> TChan (B.ByteString, Int) -> Int
  -> B.ByteString -> Websocket.Connection -> IO ()
reactRecieving lieferungenT broadcastChannel client msg connection =
  case decodeRecieving msg of
    NeueLieferung datum -> do
      (msg,lid) <- atomically $ fügeNeueLieferungHinzu lieferungenT datum
      atomically $ writeTChan broadcastChannel (msg, client)
      Websocket.sendTextData connection msg
      Websocket.sendTextData connection (zeigeNeueLieferungMsg lid)
    UpdateLieferung lieferung -> do
      msg <- atomically $ changeLieferung lieferungenT lieferung
      -- B.putStrLn $ "update lieferung with " `B.append` msg
      atomically $ writeTChan broadcastChannel (msg, 0)
    LöscheLieferung lid_del -> do
      atomically $ do
        lieferungen <- readTVar lieferungenT
        let verändert = filter ((lid_del /=) . _lid) lieferungen
        writeTVar lieferungenT verändert
      let msg = löscheLieferungMsg lid_del
      atomically $ writeTChan broadcastChannel (msg, 0)
    LöscheBestellung lid_del bid_del -> do
      atomically $ do
        lieferungen <- readTVar lieferungenT
        let überLieferung =
              over bestellungen $ filter $ (bid_del /=) . _bid
            verändert = map überLieferung lieferungen
        writeTVar lieferungenT verändert
      let msg = löscheBestellungMsg lid_del bid_del
      atomically $ writeTChan broadcastChannel (msg, 0)
    NeueBestellung lid -> do
      maybeMsg <- atomically $ fügeNeueBestellungHinzu lieferungenT lid
      case maybeMsg of
        Just msg -> atomically $ writeTChan broadcastChannel (msg, 0)
        Nothing -> return ()
    _ -> return ()

type Decoder a = A.Object -> At.Parser a

decodeRecieving :: B.ByteString -> Recieving
decodeRecieving msg =
  maybe MalformedMsg id $
    M.listToMaybe . M.catMaybes . maybe [] id $ do
      obj <- A.decode msg
      return $
        map (flip At.parseMaybe obj)
          [ decodeUpdateLieferung
          , decodeLöscheBestellung
          , decodeNeueLieferung
          , decodeLöscheLieferung
          , decodeNeueBestellung
          ]

decodeUpdateLieferung :: Decoder Recieving
decodeUpdateLieferung obj =
  do
    lieferung <- obj .: "UpdateLieferung"
    return $ UpdateLieferung lieferung

decodeLöscheBestellung :: Decoder Recieving
decodeLöscheBestellung obj =
  do
    bid <- obj .: "LöscheBestellung"
    lid <- obj .: "Lieferung"
    return $ LöscheBestellung lid bid

decodeLöscheLieferung :: Decoder Recieving
decodeLöscheLieferung obj =
  do
    lid <- obj .: "LöscheLieferung"
    return $ LöscheLieferung lid

decodeNeueBestellung :: Decoder Recieving
decodeNeueBestellung obj =
  do
    lid <- obj .: "NeueBestellung"
    return $ NeueBestellung lid

decodeNeueLieferung :: Decoder Recieving
decodeNeueLieferung obj =
  do
    time <- obj .: "NeueLieferung"
    -- let datum = vorne
    --     (hintenR,vorneR) = span (/= ':') (reverse dat)
    --     schwanz = take 2 $ reverse hintenR
    --     vorne = drop 4 $ reverse $ drop 1 vorneR
    return $ NeueLieferung time


--------------------------------------------------------------------------------------
------------------ connection stuff ---------------------------------------------------

delay = threadDelay (1000 * 1000)

sending lieferungenT fromBroadcast client connection =
  forever $ do
    -- delay
    (msg, broadcastingClient) <- atomically $ readTChan fromBroadcast
    when (client /= broadcastingClient) $ do
      Websocket.sendTextData connection msg
      -- B.putStrLn $ "sending " `B.append` msg

recieving lieferungenT broadcastChannel client connection = forever $ do
  -- threadDelay (100 * 1000)
  msg <- Websocket.receiveData connection
  reactRecieving lieferungenT broadcastChannel client msg connection
  -- B.putStrLn $ "recieving msg: " `B.append` msg

wsApp lieferungenT broadcastChannel clientT pendingConnection = do
  putStrLn "newConnection"
  connection <- acceptRequest pendingConnection
  (fromBroadcast, initMsgs) <- atomically $
     (,) <$> dupTChan broadcastChannel
         <*> initMsgsT lieferungenT
  mapM_ (Websocket.sendTextData connection) initMsgs
  client <- atomically $ do
      client <- readTVar clientT
      writeTVar clientT (client+1)
      return client
  forkIO $ recieving lieferungenT broadcastChannel client connection
  sending lieferungenT fromBroadcast client connection

-- waiApp :: Application
waiApp wsApp = WaiWebsocket.websocketsOr defaultConnectionOptions wsApp backupApp
  where
    backupApp :: Application
    backupApp _ respond = respond sendIndex

sendIndex :: Wai.Response
sendIndex = responseFile
    status200
    [("Content-Type", "text/html")]
    "frontend/index.html"
    Nothing

-------------------------------------------------------------------
------------------ file handling ---------------------------------

needToSaveT :: TVar Bool
needToSaveT =
  unsafePerformIO $ newTVarIO False

letzteLieferungenT :: TVar Bool
letzteLieferungenT =
  unsafePerformIO $ newTVarIO False

speichereLieferungenPeriodisch lieferungenT =
  forkIO $ do
    letzteLiefT <- newTVarIO =<< atomically (readTVar lieferungenT)
    forever $ do
      lieferungen <- atomically $ do
        lief <- readTVar lieferungenT
        letzteLief <- swapTVar letzteLiefT lief
        STM.check $ lief /= letzteLief
        -- STM.check =<< swapTVar needToSaveT False
        return lief
      speichereLieferungen lieferungen
      -- putStrLn "data saved"
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

---------------------------------------------------
-------------------- main ----------------------

main = do
  forkIO $ do
    mayLief <- öffneLieferungen
    flip (maybe (return ())) mayLief $ \lief -> do
      lieferungenT <- newTVarIO lief
      clientT      <- newTVarIO (1 :: Int)
      broadcastChannel <- newBroadcastTChanIO
      -- forkIO $ Websocket.runServer
      --   "localhost"
      --   3000
      speichereLieferungenPeriodisch lieferungenT
      Warp.run 3000 $
        waiApp (wsApp lieferungenT broadcastChannel clientT)
  void $ getLine
  Exit.exitSuccess
