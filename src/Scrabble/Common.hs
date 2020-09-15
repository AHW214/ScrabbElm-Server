module Scrabble.Common
  ( ID (..),
    maybeFromJSON,
    modifyTMVar',
    stateTMVar,
    withTMVar',
  )
where

import Data.Aeson (FromJSON, Result (..), ToJSON, fromJSON)
import qualified Data.Aeson as JSON
import RIO

newtype ID a = ID Text
  deriving (Eq, Ord, IsString, FromJSON, ToJSON)

withTMVar' :: TMVar a -> (a -> b) -> STM b
withTMVar' var f = do
  x <- readTMVar var
  pure $! f x
{-# INLINE withTMVar' #-}

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

maybeFromJSON :: FromJSON a => JSON.Value -> Maybe a
maybeFromJSON value =
  case fromJSON value of
    Success x ->
      Just x
    _ ->
      Nothing
