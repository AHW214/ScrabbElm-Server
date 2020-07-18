module Scrabble.Room
  ( Room (..)
  , RoomView (..)
  , addPendingClient
  , getClients
  , getPlayer
  , hasPlayer
  , hasPlayerName
  , isClientOwner
  , inGame
  , isEmpty
  , isFull
  , maxCapacity
  , new
  , registerPendingClient
  , removePendingClient
  , removePlayer
  , switchTurn
  , toView
  ) where


--------------------------------------------------------------------------------
import           Data.Text       (Text)

import           Scrabble.Client (Client (..))
import           Scrabble.Player (Player (..))
import           Scrabble.Types  (Room (..), RoomView (..))

import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe      as Maybe
import qualified Data.Set        as Set

import qualified Scrabble.Player as Player


--------------------------------------------------------------------------------
maxCapacity :: Int
maxCapacity = 4


--------------------------------------------------------------------------------
new :: Text -> Int -> Client -> Room
new name capacity owner = Room
  { roomCapacity       = capacity
  , roomName           = name
  , roomOwner          = owner
  , roomPendingClients = Set.empty
  , roomPlayers        = Map.empty
  , roomPlaying        = Nothing
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
occupancy Room { roomPendingClients, roomPlayers } =
  length roomPendingClients + length roomPlayers


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
hasPlayer :: Client -> Room -> Bool
hasPlayer client = Map.member client . roomPlayers


--------------------------------------------------------------------------------
hasPendingClient :: Client -> Room -> Bool
hasPendingClient client = Set.member client . roomPendingClients


--------------------------------------------------------------------------------
getClients :: Room -> [ Client ]
getClients = Map.keys . roomPlayers


--------------------------------------------------------------------------------
hasPlayerName :: Text -> Room -> Bool
hasPlayerName name =
  List.any (Player.hasName name) . Map.elems . roomPlayers


--------------------------------------------------------------------------------
registerPendingClient :: Client -> Text -> Room -> Maybe Room
registerPendingClient client playerName room =
  if hasPendingClient client room then
    Just $ addPlayer client playerName
         $ removePendingClient client room
  else
    Nothing


--------------------------------------------------------------------------------
addPlayer :: Client -> Text -> Room -> Room
addPlayer client playerName room@Room { roomPlayers } =
  room { roomPlayers = Map.insert client player roomPlayers }
  where
    player :: Player
    player = Player.new playerName


--------------------------------------------------------------------------------
addPendingClient :: Client -> Room -> Room
addPendingClient client room@Room { roomPendingClients } =
  room { roomPendingClients = Set.insert client roomPendingClients }


--------------------------------------------------------------------------------
switchTurn :: Player -> Room -> Room
switchTurn player room =
  room { roomPlaying = Just player }


--------------------------------------------------------------------------------
isClientOwner :: Client -> Room -> Bool
isClientOwner player = (player ==) . roomOwner


--------------------------------------------------------------------------------
removePlayer :: Client -> Room -> Room
removePlayer client room@Room { roomPlayers } =
  room { roomPlayers = Map.delete client roomPlayers }


--------------------------------------------------------------------------------
removePendingClient :: Client -> Room -> Room
removePendingClient client room@Room { roomPendingClients } =
  room { roomPendingClients = Set.delete client roomPendingClients }
