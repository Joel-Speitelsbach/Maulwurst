module Debug where

import Control.Concurrent.STM as STM
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace as Deb

connectionCounterT :: TVar Int
connectionCounterT = unsafePerformIO $ newTVarIO 1

logNewConnection :: IO ()
logNewConnection = do
  counter <- atomically $ do
    counter <- readTVar connectionCounterT
    writeTVar connectionCounterT (counter + 1)
    return counter
  putStrLn $ "new Connection " ++ show counter

trace val = Deb.trace (show val) val
