module Scrabble.Authentication.Cache
  ( Cache,
    add,
    create,
    has,
    remove,
  )
where

import RIO
import qualified RIO.Set as Set
import Scrabble.Common

data Cache g a = Cache
  { gen :: g -> (a, g),
    state :: TMVar (State g a)
  }

data State g a = State
  { seed :: g,
    uniques :: Set a
  }

add :: Ord a => Cache g a -> STM a
add Cache {gen, state} =
  stateTMVar state $ \State {seed, uniques} ->
    let (unique, newSeed) = gen seed
     in ( unique,
          State
            { seed = newSeed,
              uniques = Set.insert unique uniques
            }
        )

has :: Ord a => a -> Cache g a -> STM Bool
has unique Cache {state} =
  withTMVar' state $ Set.member unique . uniques

remove :: Ord a => a -> Cache g a -> STM ()
remove unique Cache {state} =
  modifyTMVar' state $ \s@State {uniques} ->
    s {uniques = Set.delete unique uniques}

create :: g -> (g -> (a, g)) -> STM (Cache g a)
create initSeed gen =
  Cache gen
    <$> newTMVar
      State
        { seed = initSeed,
          uniques = Set.empty
        }
