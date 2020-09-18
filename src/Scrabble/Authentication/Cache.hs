-- | Thread-safe caching with atomic key generation.
module Scrabble.Authentication.Cache
  ( Cache,
    create,
    insert,
    insertWith,
    remove,
  )
where

import RIO
import qualified RIO.Map as Map
import Scrabble.Common

-- | A thread-safe cache that atomically generates keys for inserted values.
data Cache g k v = Cache
  { -- | A function for generating keys.
    gen :: g -> (k, g),
    -- | The mutable, thread-safe state of the cache.
    state :: TMVar (State g k v)
  }

-- | The internal state of a cache.
data State g k v = State
  { -- | A seed for generating new keys.
    seed :: g,
    -- | The key-value pairs stored by the cache.
    contents :: Map k v
  }

-- | Create a value with an atomically generated key
-- and insert that value into a cache.
insertWith ::
  ( Ord k,
    MonadUnliftIO m
  ) =>
  -- | The cache to insert into.
  Cache g k v ->
  -- | A function for creating a value to insert with the generated key.
  (k -> m v) ->
  -- | The key atomically generated for the inserted value.
  m k
insertWith Cache {gen, state} withKey = do
  State {seed, contents} <- atomically $ takeTMVar state

  let (unique, newSeed) = gen seed
  value <- withKey unique

  atomically $
    putTMVar
      state
      State
        { seed = newSeed,
          contents = Map.insert unique value contents
        }

  pure unique

-- | Insert a new value into a cache.
insert ::
  Ord k =>
  -- | The value to insert.
  v ->
  -- | The cache to insert into.
  Cache g k v ->
  -- | The key atomically generated for the inserted value.
  STM k
insert value Cache {gen, state} =
  stateTMVar state $ \State {seed, contents} ->
    let (unique, newSeed) = gen seed
     in ( unique,
          State
            { seed = newSeed,
              contents = Map.insert unique value contents
            }
        )

-- | Try to remove and retrieve a value from a cache.
remove ::
  Ord k =>
  -- | The key of the value to remove.
  k ->
  -- | The cache from which to remove the value.
  Cache g k v ->
  -- | The value associated with the key, if the key existed in the cache.
  STM (Maybe v)
remove key Cache {state} =
  stateTMVar state $ \s@State {contents} ->
    case Map.lookup key contents of
      Nothing ->
        ( Nothing,
          s
        )
      value ->
        ( value,
          s {contents = Map.delete key contents}
        )

-- | Create a new cache.
create ::
  -- | An initial seed for generating keys.
  g ->
  -- | A function for generating and updating the seed.
  (g -> (k, g)) ->
  -- | A new cache.
  STM (Cache g k v)
create initSeed gen =
  Cache gen
    <$> newTMVar
      State
        { seed = initSeed,
          contents = Map.empty
        }
