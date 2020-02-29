module Tile
  ( Tile
  , defaultBag
  , shuffleBag
  , bagFromString
  ) where

import Data.Char (ord)
import Data.Aeson as AE (ToJSON, toJSON)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

data Tile
  = Blank
  | Letter Char Int

instance ToJSON Tile where
  toJSON tile =
    toJSON $
      case tile of
        Blank ->
          [ ord ' ', 0 ]
        Letter c v ->
          [ ord c, v ]

points :: [ ( Int, [ Char ] ) ]
points =
  [ ( 1,  [ 'E', 'A', 'I', 'O', 'N', 'R', 'T', 'L', 'S', 'U' ] )
  , ( 2,  [ 'D', 'G' ] )
  , ( 3,  [ 'B', 'C', 'M', 'P' ] )
  , ( 4,  [ 'F', 'H', 'V', 'W', 'Y' ] )
  , ( 5,  [ 'K' ] )
  , ( 8,  [ 'J', 'X' ] )
  , ( 10, [ 'Q', 'Z' ] )
  ]

pointsLower :: [ ( Int, [ Char ] ) ]
pointsLower =
  [ ( 1,  [ 'e', 'a', 'i', 'o', 'n', 'r', 't', 'l', 's', 'u' ] )
  , ( 2,  [ 'd', 'g' ] )
  , ( 3,  [ 'b', 'c', 'm', 'p' ] )
  , ( 4,  [ 'f', 'h', 'v', 'w', 'y' ] )
  , ( 5,  [ 'k' ] )
  , ( 8,  [ 'j', 'x' ] )
  , ( 10, [ 'q', 'z' ] )
  ]

value :: Char -> Int
value =
  check pointsLower
  where
    check map c =
      case map of
        [] -> 0
        (p, cs):rest ->
          if elem c cs then
            p
          else
            check rest c

blank :: Tile
blank = Blank

letter :: Char -> Tile
letter c = Letter c (value c)

fromChar :: Char -> Tile
fromChar c =
  case c of
    ' ' ->
      blank
    _ ->
      letter c

defaultDistribution :: [ (Char, Int) ]
defaultDistribution =
  [ ('a', 9), ('b', 2), ('c', 2), ('d', 4), ('e', 12)
  , ('f', 2), ('g', 3), ('h', 2), ('i', 9), ('j', 1)
  , ('k', 1), ('l', 4), ('m', 2), ('n', 6), ('o', 8)
  , ('p', 2), ('q', 1), ('r', 6), ('s', 4), ('t', 6)
  , ('u', 4), ('v', 2), ('w', 2), ('x', 1), ('y', 2)
  , ('z', 1), (' ', 2)
  ]

defaultBag :: [ Tile ]
defaultBag =
  makeBag defaultDistribution

makeBag :: [ ( Char, Int ) ] -> [ Tile ]
makeBag =
  foldl (\bag (c, i) ->
    replicate i (fromChar c) ++ bag
  ) []

bagFromString :: String -> [ Tile ]
bagFromString =
  map fromChar

shuffleBag :: [ Tile ] -> IO [ Tile ]
shuffleBag bag =
  shuffle' bag (length bag) <$> newStdGen
