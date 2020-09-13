module Scrabble.Common
  ( ID (..),
    modifyTMVar',
    stateTMVar,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import RIO

newtype ID a = ID Text
  deriving (Eq, Ord, IsString, FromJSON, ToJSON)

-- | Mutate the contents of a TMVar (strictly).
-- | Reference: https://github.com/haskell/stm/blob/master/Control/Concurrent/STM/TVar.hs
modifyTMVar' :: TMVar a -> (a -> a) -> STM ()
modifyTMVar' var f = do
  x <- takeTMVar var
  putTMVar var $! f x
{-# INLINE modifyTMVar' #-}

-- | Reference: https://github.com/haskell/stm/blob/master/Control/Concurrent/STM/TVar.hs
stateTMVar :: TMVar s -> (s -> (a, s)) -> STM a
stateTMVar var f = do
  s <- takeTMVar var
  let (a, s') = f s
  putTMVar var s'
  pure a
{-# INLINE stateTMVar #-}
