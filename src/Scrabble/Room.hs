module Scrabble.Room
  ( Room (..)
  , RoomPreview (..)
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
  , toPreview
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson            (ToJSON, (.=))
import           Data.Map.Strict       (Map)
import           Data.Text             (Text)

import           Scrabble.Client       (Client (..))
import           Scrabble.Player       (Player (..))
import           Scrabble.Room.Preview (RoomPreview (..))

import qualified Data.Aeson            as JSON
import qualified Data.List             as List
import qualified Data.Map.Strict       as Map
import qualified Data.Maybe            as Maybe


--------------------------------------------------------------------------------
data Room = Room
  { roomCapacity :: Int
  , roomName     :: Text
  , roomPlayers  :: Map Client Player
  , roomPlaying  :: Maybe Player
  }


--------------------------------------------------------------------------------
instance ToJSON Room where
  toJSON Room { roomCapacity, roomName, roomPlayers, roomPlaying } =
    JSON.object
      [ "roomCapacity" .= roomCapacity
      , "roomName"     .= roomName
      , "roomPlayers"  .= Map.elems roomPlayers
      , "roomPlaying"  .= roomPlaying
      ]

  toEncoding Room { roomCapacity, roomName, roomPlayers, roomPlaying } =
    JSON.pairs
      $  "roomCapacity" .= roomCapacity
      <> "roomName"     .= roomName
      <> "roomPlayers"  .= Map.elems roomPlayers
      <> "roomPlaying"  .= roomPlaying


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
toPreview :: Room -> RoomPreview
toPreview room@Room { roomCapacity, roomName } = RoomPreview
  { roomPreviewCapacity  = roomCapacity
  , roomPreviewInGame    = inGame room
  , roomPreviewName      = roomName
  , roomPreviewOccupancy = occupancy room
  }


--------------------------------------------------------------------------------
inGame :: Room -> Bool
inGame = Maybe.isJust . roomPlaying


--------------------------------------------------------------------------------
occupancy :: Room -> Int
occupancy = length . roomPlayers


--------------------------------------------------------------------------------
isFull :: Room -> Bool
isFull room@Room { roomCapacity } =
  occupancy room >= roomCapacity


--------------------------------------------------------------------------------
isEmpty :: Room -> Bool
isEmpty room =
  occupancy room <= 0


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
