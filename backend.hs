{-# LANGUAGE TemplateHaskell,OverloadedStrings,ScopedTypeVariables #-}

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
import Data.Maybe (listToMaybe,isNothing,catMaybes)
import qualified Data.Char as Char
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import System.IO.Unsafe (unsafePerformIO)
import qualified Debug
import Debug (trace)
import Data.Monoid
import Foreign.C.Types as CTypes
import Misc (whenJust,doNothing)
import qualified Data.UnixTime as UnixTime

data Lieferung = Lieferung
  { _bestelldatum     :: Double
  , _lieferdatum      :: String
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
        then Just $ löscheBestellungMsg (_lid lieferung) (_bid bestellung)
        else Nothing
  )


---------------------------------------------------------------
--------------- sending stuff -------------------------------

changeLieferung :: TVar [Lieferung] -> Lieferung -> STM [B.ByteString]
changeLieferung lieferungenT lieferung =
  do
    lieferungen <- readTVar lieferungenT
    let
      (lieferungCleaned,deleteMsgs) = cleanUpLieferung lieferung
      veränderteLieferungen =
        lieferungCleaned : filter ((_lid lieferungCleaned /=) . _lid) lieferungen
    writeTVar lieferungenT veränderteLieferungen
    return $ deleteMsgs ++ [A.encode lieferungCleaned]

fügeNeueLieferungHinzu :: TVar [Lieferung] -> Double -> STM (B.ByteString, Int)
fügeNeueLieferungHinzu lieferungenT millisecs =
  do
    lieferungen <- readTVar lieferungenT
    let
      newID = (+1) $ maximum $ (0:) $ map _lid lieferungen
      pdata = PartyserviceData "" "" "" ""
      neueLieferung = Lieferung millisecs "" "" Merchingen pdata Nothing newID []
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
  | PapierkorbLieferung Int Bool
  | LöscheBestellung Int Int
  | NeueLieferung
  | NeueBestellung Int
  | MalformedMsg B.ByteString
  deriving (Show)

reactRecieving
  :: TVar [Lieferung] -> TChan (B.ByteString, Int) -> Int
  -> B.ByteString -> Websocket.Connection -> IO ()
reactRecieving lieferungenT broadcastChannel client msg connection =
  case decodeRecieving msg of
    NeueLieferung -> do
      millisecs <- getEpochMillisecs
      (msg,lid) <- atomically $ do
        (msg,lid) <- fügeNeueLieferungHinzu lieferungenT millisecs
        writeTChan broadcastChannel (msg, client)
        return (msg,lid)
      Websocket.sendTextData connection (zeigeNeueLieferungMsg lid)
    UpdateLieferung lieferung -> atomically $ do
        msgs <- changeLieferung lieferungenT lieferung
        forM_ msgs $ \msg ->
          writeTChan broadcastChannel (msg, 0)
    PapierkorbLieferung lid_pap bool -> do
      pap <- do
        if bool
          then Just <$> getEpochMillisecs
          else return Nothing
      atomically $ do
        lieferungen <-readTVar lieferungenT
        let maybeLieferung = do
              l <- listToMaybe $ filter (\l -> _lid l == lid_pap) lieferungen
              return $ l { _inPapierkorb = pap}
        whenJust maybeLieferung $ \lieferung -> do
          msgs <- changeLieferung lieferungenT lieferung
          forM_ msgs $ \msg ->
            writeTChan broadcastChannel (msg, 0)
    LöscheBestellung lid_del bid_del -> atomically $ do
      let überLieferung =
            over bestellungen $ filter $ (bid_del /=) . _bid
          msg = löscheBestellungMsg lid_del bid_del
      modifyTVar lieferungenT $ map überLieferung
      writeTChan broadcastChannel (msg, 0)
    NeueBestellung lid -> atomically $ do
      maybeMsg <- fügeNeueBestellungHinzu lieferungenT lid
      case maybeMsg of
        Just msg -> writeTChan broadcastChannel (msg, 0)
        Nothing -> return ()
    MalformedMsg msg -> do
      B.putStrLn $ "recieving malformed msg: " <> msg

getEpochMillisecs :: IO Double
getEpochMillisecs = do
  now <- UnixTime.getUnixTime
  let CTypes.CTime seconds = UnixTime.toEpochTime now
  return $ fromIntegral $ seconds * 1000

type Decoder a = A.Object -> At.Parser a

decodeRecieving :: B.ByteString -> Recieving
decodeRecieving msg =
  maybe (MalformedMsg msg) id $
    M.listToMaybe . M.catMaybes . maybe [] id $ do
      obj <- A.decode msg
      return $
        map (flip At.parseMaybe obj)
          [ decodeUpdateLieferung
          , decodeLöscheBestellung
          , decodePapierkorbLieferung
          , decodeNeueBestellung
          , decodeNeueLieferung
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


--------------------------------------------------------------------------------------
------------------ connection stuff ---------------------------------------------------

delay = threadDelay (1000 * 1000)

sending lieferungenT fromBroadcast client connection =
  forever $ do
    (msg, broadcastingClient) <- atomically $ readTChan fromBroadcast
    Websocket.sendTextData connection msg

recieving lieferungenT broadcastChannel client connection = forever $ do
  msg <- Websocket.receiveData connection
  reactRecieving lieferungenT broadcastChannel client msg connection

wsApp lieferungenT broadcastChannel clientT pendingConnection = do
  Debug.logNewConnection
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

-- waiApp :: .. -> Application
waiApp wsApp = WaiWebsocket.websocketsOr defaultConnectionOptions wsApp sendHtmlApp

sendHtmlApp :: Application
sendHtmlApp _ respond = respond sendIndex

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

---------------------------------------------------
-------------------- main ----------------------

main = do
  mayLief <- öffneLieferungen
  flip (maybe $ Exit.exitFailure) mayLief $ \lief -> do
    void . forkIO $ do
        lieferungenT <- newTVarIO lief
        clientT      <- newTVarIO (1 :: Int)
        broadcastChannel <- newBroadcastTChanIO
        speichereLieferungenPeriodisch lieferungenT
        forkIO $ Websocket.runServer
          "192.168.178.45"
          18539
          (wsApp lieferungenT broadcastChannel clientT)
        Warp.run 3000 sendHtmlApp
    void $ getLine
    Exit.exitSuccess
