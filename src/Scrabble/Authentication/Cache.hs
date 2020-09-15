module Scrabble.Authentication.Cache
  ( Cache,
    add,
    has,
    remove,
  )
where

import RIO
import qualified RIO.Set as Set
import Scrabble.Common

newtype Cache a = Cache (TMVar (Set a))

add :: Ord a => a -> Cache a -> STM ()
add x (Cache c) =
  modifyTMVar' c $ Set.insert x

has :: Ord a => a -> Cache a -> STM Bool
has x (Cache c) =
  withTMVar' c $ Set.member x

remove :: Ord a => a -> Cache a -> STM ()
remove x (Cache c) =
  modifyTMVar' c $ Set.delete x
