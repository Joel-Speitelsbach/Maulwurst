module Debug where

import Control.Concurrent.STM as STM
import Control.Concurrent.STM.TVar
import System.IO.Unsafe (unsafePerformIO)

connectionCounterT :: TVar Int
connectionCounterT = unsafePerformIO $ newTVarIO 1

logNewConnection :: IO ()
logNewConnection = do
  counter <- atomically $ do
    counter <- readTVar connectionCounterT
    writeTVar connectionCounterT (counter + 1)
    return counter
  putStrLn $ "new Connection " ++ show counter
