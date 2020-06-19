module Tile
  ( Tile
  , bagFromString
  , defaultBag
  , shuffleBag
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson            (ToJSON (toJSON))
import           Data.Char             (ord)
import qualified Data.List             as List
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe)
import           System.Random         (getStdRandom)


--------------------------------------------------------------------------------
import           Random                (shuffleList)


--------------------------------------------------------------------------------
data Tile
  = Blank
  | Letter Char Int


--------------------------------------------------------------------------------
instance ToJSON Tile where
  toJSON = toJSON .
    \case
      Blank ->
        [ ord ' ', 0 ]

      Letter c v ->
        [ ord c, v ]


--------------------------------------------------------------------------------
points :: Map Char Int
points =
  List.foldl' (\mp ( pts, cs ) -> List.foldl' (withPoints pts) mp cs) Map.empty groups
  where
    withPoints pts mp chr =
      Map.insert chr pts mp

    groups =
      [ ( 1,  [ 'e', 'a', 'i', 'o', 'n', 'r', 't', 'l', 's', 'u' ] )
      , ( 2,  [ 'd', 'g' ] )
      , ( 3,  [ 'b', 'c', 'm', 'p' ] )
      , ( 4,  [ 'f', 'h', 'v', 'w', 'y' ] )
      , ( 5,  [ 'k' ] )
      , ( 8,  [ 'j', 'x' ] )
      , ( 10, [ 'q', 'z' ] )
      ]


--------------------------------------------------------------------------------
value :: Char -> Int
value =
  fromMaybe 0 . flip Map.lookup points


--------------------------------------------------------------------------------
blank :: Tile
blank = Blank


--------------------------------------------------------------------------------
letter :: Char -> Tile
letter c = Letter c $ value c


--------------------------------------------------------------------------------
fromChar :: Char -> Tile
fromChar c =
  case c of
    ' ' ->
      blank

    _ ->
      letter c


--------------------------------------------------------------------------------
defaultDistribution :: [ (Char, Int) ]
defaultDistribution =
  [ ( 'a', 9 ), ( 'b', 2 ), ( 'c', 2 ), ( 'd', 4 ), ( 'e', 12 )
  , ( 'f', 2 ), ( 'g', 3 ), ( 'h', 2 ), ( 'i', 9 ), ( 'j', 1 )
  , ( 'k', 1 ), ( 'l', 4 ), ( 'm', 2 ), ( 'n', 6 ), ( 'o', 8 )
  , ( 'p', 2 ), ( 'q', 1 ), ( 'r', 6 ), ( 's', 4 ), ( 't', 6 )
  , ( 'u', 4 ), ( 'v', 2 ), ( 'w', 2 ), ( 'x', 1 ), ( 'y', 2 )
  , ( 'z', 1 ), ( ' ', 2 )
  ]


--------------------------------------------------------------------------------
defaultBag :: [ Tile ]
defaultBag =
  makeBag defaultDistribution


--------------------------------------------------------------------------------
makeBag :: [ ( Char, Int ) ] -> [ Tile ]
makeBag =
  foldl (\bag ( c, i ) -> replicate i (fromChar c) ++ bag) []


--------------------------------------------------------------------------------
bagFromString :: String -> [ Tile ]
bagFromString = map fromChar


--------------------------------------------------------------------------------
shuffleBag :: [ Tile ] -> IO [ Tile ]
shuffleBag = getStdRandom . shuffleList
