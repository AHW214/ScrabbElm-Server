{- https://wiki.haskell.org/Random_shuffle -}

module Scrabble.Random
  ( randomSequence,
    randomRSequence,
    shuffleList,
  )
where

--------------------------------------------------------------------------------
import Data.Map (Map, (!))
import qualified Data.Map as Map
import System.Random (Random, RandomGen, random, randomR)

--------------------------------------------------------------------------------
randomSequence :: (Random a, RandomGen g) => Int -> g -> ([a], g)
randomSequence = randomSequenceHelper random

--------------------------------------------------------------------------------
randomRSequence :: (Random a, RandomGen g) => (a, a) -> Int -> g -> ([a], g)
randomRSequence = randomSequenceHelper . randomR

--------------------------------------------------------------------------------
randomSequenceHelper :: forall a g. (g -> (a, g)) -> Int -> g -> ([a], g)
randomSequenceHelper rand len gen =
  collect ([], gen) len
  where
    collect :: ([a], g) -> Int -> ([a], g)
    collect acc@(rs, g) n =
      if n <= 0
        then acc
        else
          let (r, g') = rand g
           in collect (r : rs, g') (n - 1)

--------------------------------------------------------------------------------
shuffleList :: RandomGen g => [a] -> g -> ([a], g)
shuffleList = flip fisherYates

--------------------------------------------------------------------------------
fisherYatesStep ::
  RandomGen g =>
  (Map Int a, g) ->
  (Int, a) ->
  (Map Int a, g)
fisherYatesStep (m, gen) (i, x) =
  (Map.insert j x $ Map.insert i (m ! j) m, gen')
  where
    (j, gen') =
      randomR (0, i) gen

--------------------------------------------------------------------------------
fisherYates :: forall a g. RandomGen g => g -> [a] -> ([a], g)
fisherYates gen xs =
  case xs of
    [] ->
      ([], gen)
    first : rest ->
      toElems $ foldl fisherYatesStep (initial first gen) (numerate rest)
  where
    toElems :: (Map Int a, g) -> ([a], g)
    toElems (x, y) = (Map.elems x, y)

    numerate :: [a] -> [(Int, a)]
    numerate = zip [1 ..]

    initial :: a -> g -> (Map Int a, g)
    initial x g = (Map.singleton 0 x, g)
