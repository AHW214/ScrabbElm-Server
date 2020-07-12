module Scrabble.Room
  ( Room (..)
  , RoomEvent (..)
  , RoomView (..)
  , addPlayer
  , getClients
  , getPlayer
  , hasPlayerTag
  , inGame
  , isEmpty
  , isFull
  , isPlayerOwner
  , maxCapacity
  , new
  , removeClient
  , removePlayer
  , switchTurn
  , toView
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (TBQueue)
import           Data.Aeson             (ToJSON, (.=))
import           Data.Map.Strict        (Map)
import           Data.Text              (Text)

import           Scrabble.Client        (Client (..))
import           Scrabble.Player        (Player (..))

import qualified Data.Aeson             as JSON
import qualified Data.List              as List
import qualified Data.Map.Strict        as Map
import qualified Data.Maybe             as Maybe


--------------------------------------------------------------------------------
data Room = Room
  { roomCapacity :: Int
  , roomName     :: Text
  , roomOwner    :: Player
  , roomPlayers  :: Map Client Player
  , roomPlaying  :: Maybe Player
  , roomQueue    :: TBQueue RoomEvent
  }


--------------------------------------------------------------------------------
data RoomView = RoomView
  { roomViewCapacity  :: Int
  , roomViewInGame    :: Bool
  , roomViewName      :: Text
  , roomViewOccupancy :: Int
  , roomViewQueue     :: TBQueue RoomEvent
  }


--------------------------------------------------------------------------------
data RoomEvent
  = RoomPlayerJoin Text Client
  | RoomPlayerLeave Client


--------------------------------------------------------------------------------
instance ToJSON Room where
  toJSON Room
    { roomCapacity
    , roomName
    , roomOwner
    , roomPlayers
    , roomPlaying
    } = JSON.object
          [ "roomCapacity" .= roomCapacity
          , "roomName"     .= roomName
          , "roomOwner"    .= roomOwner
          , "roomPlayers"  .= Map.elems roomPlayers
          , "roomPlaying"  .= roomPlaying
          ]

  toEncoding Room
    { roomCapacity
    , roomName
    , roomOwner
    , roomPlayers
    , roomPlaying
    } = JSON.pairs
          $  "roomCapacity" .= roomCapacity
          <> "roomName"     .= roomName
          <> "roomOwner"    .= roomOwner
          <> "roomPlayers"  .= Map.elems roomPlayers
          <> "roomPlaying"  .= roomPlaying


--------------------------------------------------------------------------------
instance ToJSON RoomView where
  toJSON RoomView
    { roomViewCapacity
    , roomViewInGame
    , roomViewName
    , roomViewOccupancy
    } = JSON.object
          [ "roomViewCapacity"  .= roomViewCapacity
          , "roomViewInGame"    .= roomViewInGame
          , "roomViewName"      .= roomViewName
          , "roomViewOccupancy" .= roomViewOccupancy
          ]

  toEncoding RoomView
    { roomViewCapacity
    , roomViewInGame
    , roomViewName
    , roomViewOccupancy
    } = JSON.pairs
          $ "roomViewCapacity"   .= roomViewCapacity
          <> "roomViewInGame"    .= roomViewInGame
          <> "roomViewName"      .= roomViewName
          <> "roomViewOccupancy" .= roomViewOccupancy


--------------------------------------------------------------------------------
maxCapacity :: Int
maxCapacity = 4


--------------------------------------------------------------------------------
new :: Text -> Int -> Player -> TBQueue RoomEvent -> Room
new roomName roomCapacity roomOwner roomQueue = Room
  { roomCapacity
  , roomName
  , roomOwner
  , roomPlayers  = Map.empty
  , roomPlaying  = Nothing
  , roomQueue
  }


--------------------------------------------------------------------------------
toView :: Room -> RoomView
toView room@Room { roomCapacity, roomName, roomQueue } = RoomView
  { roomViewCapacity  = roomCapacity
  , roomViewInGame    = inGame room
  , roomViewName      = roomName
  , roomViewOccupancy = occupancy room
  , roomViewQueue     = roomQueue
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
getPlayer client = Map.lookup client . roomPlayers


--------------------------------------------------------------------------------
getClients :: Room -> [ Client ]
getClients = Map.keys . roomPlayers


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
isPlayerOwner :: Player -> Room -> Bool
isPlayerOwner player = (player ==) . roomOwner


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
