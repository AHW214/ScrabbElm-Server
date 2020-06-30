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
  , removeClient
  , removePlayer
  , switchTurn
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson       (FromJSON, ToJSON, withObject, (.=), (.:))
import           Data.Map.Strict  (Map)
import           Data.Text        (Text)

import           Scrabble.Client  (Client (..))
import           Scrabble.Player  (Player (..))

import qualified Data.Aeson       as JSON
import qualified Data.List        as List
import qualified Data.Map.Strict  as Map
import qualified Data.Maybe       as Maybe


--------------------------------------------------------------------------------
data Room = Room
  { roomCapacity :: Int
  , roomName     :: Text
  , roomPlayers  :: Map Client Player
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
getPlayer :: Client -> Room -> Maybe Player
getPlayer client =
  Map.lookup client . roomPlayers


--------------------------------------------------------------------------------
hasPlayerTag :: Text -> Room -> Bool
hasPlayerTag tag =
  List.any ((tag ==) . playerName) . Map.elems . roomPlayers


--------------------------------------------------------------------------------
addPlayer :: Player -> Room -> Room
addPlayer player@Player { playerClient } room@Room { roomPlayers } =
  room { roomPlayers = Map.insert playerClient player roomPlayers }


--------------------------------------------------------------------------------
removePlayer :: Player -> Room -> Maybe Room
removePlayer = removeClient . playerClient


--------------------------------------------------------------------------------
switchTurn :: Player -> Room -> Room
switchTurn player room =
  room { roomPlaying = Just player }


--------------------------------------------------------------------------------
removeClient :: Client -> Room -> Maybe Room
removeClient client room@Room { roomPlayers } =
  if null players then
    Nothing
  else
    Just $ room { roomPlayers = players }
  where
    players =
      Map.delete client roomPlayers
