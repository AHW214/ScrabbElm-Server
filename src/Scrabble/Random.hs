{- https://wiki.haskell.org/Random_shuffle -}

module Scrabble.Random
  ( randomSequence
  , randomRSequence
  , shuffleList
  ) where


--------------------------------------------------------------------------------
import           Data.Map      (Map, (!))
import           System.Random (Random, RandomGen, random, randomR)

import qualified Data.Map      as Map


--------------------------------------------------------------------------------
fisherYatesStep
  :: RandomGen g
  => ( Map Int a, g )
  -> ( Int, a )
  -> ( Map Int a, g )
fisherYatesStep ( m, gen ) ( i, x ) =
  ( Map.insert j x $ Map.insert i (m ! j) m, gen' )
  where
    ( j, gen' ) =
      randomR ( 0, i ) gen


--------------------------------------------------------------------------------
fisherYates :: RandomGen g => g -> [ a ] -> ( [ a ], g )
fisherYates gen xs =
  case xs of
    [] ->
      ( [], gen )

    first:rest ->
      toElems $ foldl fisherYatesStep (initial first gen) (numerate rest)
      where
        toElems ( x, y ) = ( Map.elems x, y )
        numerate = zip [1..]
        initial x g = ( Map.singleton 0 x, g )


--------------------------------------------------------------------------------
shuffleList :: RandomGen g => [ a ] -> g -> ( [ a ], g )
shuffleList = flip fisherYates


--------------------------------------------------------------------------------
randomSequenceHelper :: (g -> ( a, g )) -> Int -> g -> ( [ a ], g )
randomSequenceHelper rand len gen =
  collect ( [], gen ) len
  where
    collect acc@( rs, g ) n =
      if n <= 0 then
        acc
      else
        let
          ( r, g' ) = rand g
        in
          collect ( r:rs, g' ) (n - 1)


--------------------------------------------------------------------------------
randomRSequence :: (Random a, RandomGen g) => ( a, a ) -> Int -> g -> ( [ a ], g )
randomRSequence =
  randomSequenceHelper . randomR


--------------------------------------------------------------------------------
randomSequence :: (Random a, RandomGen g) => Int -> g -> ( [ a ], g )
randomSequence =
  randomSequenceHelper random
