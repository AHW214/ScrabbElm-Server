-- | Common types and functions. TODO: Convert this to a prelude type module?
module Scrabble.Common
  ( ID (..),
    Try (..),
    maybeFromJSON,
    modifyTMVar',
    stateTMVar,
    tries,
    withTMVar',
  )
where

import Data.Aeson (FromJSON, Result (..), ToJSON, fromJSON)
import qualified Data.Aeson as JSON
import RIO

data Try e where
  Try :: Exception e' => (e' -> e) -> Try e

-- | A text identifier for some entity.
newtype ID a = ID Text
  deriving (Eq, Ord, IsString, FromJSON, ToJSON)

-- | Strictly operate on the contents of a TMVar.
withTMVar' :: TMVar a -> (a -> b) -> STM b
withTMVar' var f = do
  x <- readTMVar var
  pure $! f x
{-# INLINE withTMVar' #-}

-- | Strictly mutate the contents of a TMVar.
-- Reference: https://github.com/haskell/stm/blob/master/Control/Concurrent/STM/TVar.hs
modifyTMVar' :: TMVar a -> (a -> a) -> STM ()
modifyTMVar' var f = do
  x <- takeTMVar var
  putTMVar var $! f x
{-# INLINE modifyTMVar' #-}

-- | Strictly mutate the contents of a TMVar and return additional output.
-- Reference: https://github.com/haskell/stm/blob/master/Control/Concurrent/STM/TVar.hs
stateTMVar :: TMVar s -> (s -> (a, s)) -> STM a
stateTMVar var f = do
  s <- takeTMVar var
  let (a, s') = f s
  putTMVar var s'
  pure a
{-# INLINE stateTMVar #-}

-- | Convert a value from JSON, but return a Maybe instead of aeson's Result.
maybeFromJSON :: FromJSON a => JSON.Value -> Maybe a
maybeFromJSON value =
  case fromJSON value of
    Success x ->
      Just x
    _ ->
      Nothing

tries :: MonadUnliftIO m => m a -> [Try e] -> m (Either e a)
tries action =
  catches (Right <$> action)
    . fmap (\(Try t) -> Handler (pure . Left . t))
