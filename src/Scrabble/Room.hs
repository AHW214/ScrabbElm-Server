module Scrabble.Room
  ( Room (..)
  , addPlayer
  , empty
  , getPlayer
  , hasPlayerTag
  , inGame
  , isEmpty
  , isFull
  , maxCapacity
  , new
  , removePlayer
  , switchTurn
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson       (FromJSON, ToJSON, withObject, (.=), (.:))
import           Data.Map.Strict  (Map)
import           Data.Text        (Text)

import           Scrabble.Player  (Player (..))

import qualified Data.Aeson       as JSON
import qualified Data.List        as List
import qualified Data.Map.Strict  as Map
import qualified Data.Maybe       as Maybe


--------------------------------------------------------------------------------
data Room = Room
  { roomCapacity :: Int
  , roomId       :: Int
  , roomName     :: Text
  , roomPlayers  :: Map Text Player
  , roomPlaying  :: Maybe Player
  }


--------------------------------------------------------------------------------
instance ToJSON Room where
  toJSON room =
    JSON.object
      [ "roomName"       .= roomName room
      , "roomCapacity"   .= roomCapacity room
      , "roomNumPlayers" .= numPlayers room
      , "roomInGame"     .= inGame room
      ]

  toEncoding room =
    JSON.pairs
      $  "roomName"       .= roomName room
      <> "roomCapacity"   .= roomCapacity room
      <> "roomNumPlayers" .= numPlayers room
      <> "roomInGame"     .= inGame room


--------------------------------------------------------------------------------
instance FromJSON Room where
  parseJSON = withObject "Room" $ \v -> do
    roomName     <- v .: "roomName"
    roomCapacity <- v .: "roomCapacity"

    pure $ empty { roomName, roomCapacity }


--------------------------------------------------------------------------------
maxCapacity :: Int
maxCapacity = 4


--------------------------------------------------------------------------------
empty :: Room
empty = Room
  { roomCapacity = maxCapacity
  , roomId       = 0
  , roomName     = ""
  , roomPlayers  = Map.empty
  , roomPlaying  = Nothing
  }


--------------------------------------------------------------------------------
new :: Text -> Int -> Room
new roomName roomCapacity =
  empty { roomCapacity, roomName }


--------------------------------------------------------------------------------
inGame :: Room -> Bool
inGame = Maybe.isJust . roomPlaying


--------------------------------------------------------------------------------
numPlayers :: Room -> Int
numPlayers = length . roomPlayers


--------------------------------------------------------------------------------
isFull :: Room -> Bool
isFull room@Room { roomCapacity } =
  numPlayers room >= roomCapacity


--------------------------------------------------------------------------------
isEmpty :: Room -> Bool
isEmpty room =
  numPlayers room <= 0


--------------------------------------------------------------------------------
getPlayer :: Text -> Room -> Maybe Player
getPlayer name =
  Map.lookup name . roomPlayers


--------------------------------------------------------------------------------
hasPlayerTag :: Text -> Room -> Bool
hasPlayerTag tag =
  List.any ((tag ==) . playerName) . Map.elems . roomPlayers


--------------------------------------------------------------------------------
addPlayer :: Player -> Room -> Room
addPlayer player@Player { playerClient = ( ticket, _ ) } room@Room { roomPlayers } =
  room { roomPlayers = Map.insert ticket player roomPlayers }


--------------------------------------------------------------------------------
removePlayer :: Player -> Room -> Room
removePlayer Player { playerClient = ( ticket, _ ) } room@Room { roomPlayers } =
  room { roomPlayers = Map.delete ticket roomPlayers }


--------------------------------------------------------------------------------
switchTurn :: Player -> Room -> Room
switchTurn player room =
  room { roomPlaying = Just player }
