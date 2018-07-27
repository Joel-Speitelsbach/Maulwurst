module Misc where

import qualified Data.UnixTime as UnixTime
import Foreign.C.Types as CTypes

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust may consumer =
  maybe doNothing consumer may

doNothing :: Monad m => m ()
doNothing = return ()

withDefault :: a -> Maybe a -> a
withDefault def may =
  maybe def id may

getEpochMillisecs :: IO Double
getEpochMillisecs = do
  now <- UnixTime.getUnixTime
  let CTypes.CTime seconds = UnixTime.toEpochTime now
  return $ fromIntegral $ seconds * 1000
