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

data Cache a = Cache
  { createUnique :: a -> a,
    state :: TMVar (State a)
  }

data State a = State
  { currentUnique :: a,
    uniques :: Set a
  }

add :: Ord a => Cache a -> STM a
add Cache {createUnique, state} =
  stateTMVar state $ \State {currentUnique, uniques} ->
    let newUnique = createUnique currentUnique
     in ( newUnique,
          State
            { currentUnique = newUnique,
              uniques = Set.insert newUnique uniques
            }
        )

has :: Ord a => a -> Cache a -> STM Bool
has unique Cache {state} =
  withTMVar' state $ Set.member unique . uniques

remove :: Ord a => a -> Cache a -> STM ()
remove unique Cache {state} =
  modifyTMVar' state $ \s@State {uniques} ->
    s {uniques = Set.delete unique uniques}

create :: a -> (a -> a) -> STM (Cache a)
create initialUnique createUnique =
  Cache createUnique
    <$> newTMVar
      State
        { currentUnique = initialUnique,
          uniques = Set.empty
        }
