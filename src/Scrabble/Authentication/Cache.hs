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

data Cache g k v = Cache
  { gen :: g -> (k, g),
    state :: TMVar (State g k v)
  }

data State g k v = State
  { seed :: g,
    contents :: Map k v
  }

insertWith :: (Ord k, MonadUnliftIO m) => Cache g k v -> (k -> m v) -> m k
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

insert :: Ord k => v -> Cache g k v -> STM k
insert value Cache {gen, state} =
  stateTMVar state $ \State {seed, contents} ->
    let (unique, newSeed) = gen seed
     in ( unique,
          State
            { seed = newSeed,
              contents = Map.insert unique value contents
            }
        )

remove :: Ord k => k -> Cache g k v -> STM (Maybe v)
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

create :: g -> (g -> (k, g)) -> STM (Cache g k v)
create initSeed gen =
  Cache gen
    <$> newTMVar
      State
        { seed = initSeed,
          contents = Map.empty
        }
