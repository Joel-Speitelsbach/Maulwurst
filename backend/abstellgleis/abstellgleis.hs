
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
wsApp :: PendingConnection -> IO ()
wsApp pendingConnection = do
  putStrLn "newConnection"
  connection <- acceptRequest pendingConnection
  forkIO $ recieving connection
  sending connection
  -- threads <- mapM (\act -> forkIO $ act connection) [sending, recieving]
  -- void $ getLine
  -- threadDelay (10 * 1000 * 1000)
  -- mapM_ killThread threads


tlsSettings = WarpTLS.tlsSettings "certificates/certificate.pem" "certificates/key.pem"


mainTls = do
  WarpTLS.runTLS tlsSettings Warp.defaultSettings app
