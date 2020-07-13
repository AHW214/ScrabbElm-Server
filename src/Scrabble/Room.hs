module Scrabble.Room
  ( Room (..)
  , RoomEventExternal (..)
  , RoomEventInternal (..)
  , RoomEvent
  , RoomQueue
  , RoomView (..)
  , addPlayer
  , getClients
  , getPlayer
  , hasPlayerName
  , inGame
  , isEmpty
  , isFull
  , isPlayerOwner
  , maxCapacity
  , new
  , removePlayer
  , switchTurn
  , toView
  ) where


--------------------------------------------------------------------------------
import           Data.Text       (Text)

import           Scrabble.Client (Client (..))
import           Scrabble.Player (Player (..))
import           Scrabble.Types  (Room (..), RoomEventExternal (..),
                                  RoomEventInternal (..), RoomEvent,
                                  RoomQueue, RoomView (..))

import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe      as Maybe

import qualified Scrabble.Player as Player


--------------------------------------------------------------------------------
maxCapacity :: Int
maxCapacity = 4


--------------------------------------------------------------------------------
new :: Text -> Int -> Player -> Room
new name capacity owner = Room
  { roomCapacity = capacity
  , roomName     = name
  , roomOwner    = owner
  , roomPlayers  = Map.empty
  , roomPlaying  = Nothing
  }


--------------------------------------------------------------------------------
toView :: Room -> RoomView
toView room@Room { roomCapacity, roomName } = RoomView
  { roomViewCapacity  = roomCapacity
  , roomViewInGame    = inGame room
  , roomViewName      = roomName
  , roomViewOccupancy = occupancy room
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
hasPlayerName :: Text -> Room -> Bool
hasPlayerName name =
  List.any (Player.hasName name) . Map.elems . roomPlayers


--------------------------------------------------------------------------------
addPlayer :: Client -> Text -> Room -> Room
addPlayer client name room@Room { roomPlayers } =
  room { roomPlayers = Map.insert client player roomPlayers }
  where
    player :: Player
    player = Player.new name


--------------------------------------------------------------------------------
switchTurn :: Player -> Room -> Room
switchTurn player room =
  room { roomPlaying = Just player }


--------------------------------------------------------------------------------
isPlayerOwner :: Player -> Room -> Bool
isPlayerOwner player = (player ==) . roomOwner


--------------------------------------------------------------------------------
removePlayer :: Client -> Room -> Maybe Room
removePlayer client room@Room { roomPlayers } =
  if null players then
    Nothing
  else
    Just $ room { roomPlayers = players }
  where
    players =
      Map.delete client roomPlayers
