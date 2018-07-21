module Misc where

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust may consumer =
  maybe doNothing consumer may

doNothing :: Monad m => m ()
doNothing = return ()

withDefault :: a -> Maybe a -> a
withDefault def may =
  maybe def id may
