
import Prelude hiding (read)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad
import Data.IORef

mchan = (newChan,dupChan,writeChan,readChan)
tchan = (newBroadcastTChanIO,atomically . dupTChan,atomically % writeTChan,atomically . readTChan)

(%) f g a b = f (g a b)

(new,dup,write,read) = tchan

send chan =
  do
    counter <- newIORef 0
    flip mapM_ [1 :: Integer ..] $ \c ->
      write chan c
    write chan (-5)

recieve chan =
  forever $ do
    i <- read chan
    return ()
    -- when (i /= (-5)) $
    --   recieve chan

spawn rootchan =
  do
    chan <- dup rootchan
    forkIO $ send chan
    -- forkIO $ recieve chan
    return chan

main =
  do
    rootchan <- new
    chan <- dup rootchan
    write chan 7
    print =<< read chan


delay = threadDelay (1000 * 1)
