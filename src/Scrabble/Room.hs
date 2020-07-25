module Scrabble.Room
  ( Room (..)
  , addPlayer
  , isEmpty
  , isFull
  , new
  , occupancy
  , removePlayer
  ) where


--------------------------------------------------------------------------------
import           Scrabble.ClientRoom (Client (..), Room (..))
import           Scrabble.Common     (ID)
import           Scrabble.Player     (Player)

import qualified Data.Map            as Map

import qualified Scrabble.Player     as Player


--------------------------------------------------------------------------------
new :: ID Room -> Int -> Room
new roomId roomCapacity = Room
  { roomCapacity
  , roomId
  , roomPlayers = Map.empty
  }


--------------------------------------------------------------------------------
addPlayer :: Client -> ID Player -> Room -> Room
addPlayer Client { clientId } playerId room@Room { roomPlayers } = room
  { roomPlayers = Map.insert clientId player roomPlayers
  }
  where
    player :: Player
    player = Player.new playerId


--------------------------------------------------------------------------------
removePlayer :: Client -> Room -> Room
removePlayer Client { clientId } room@Room { roomPlayers } = room
  { roomPlayers = Map.delete clientId roomPlayers
  }


--------------------------------------------------------------------------------
isEmpty :: Room -> Bool
isEmpty = null . roomPlayers


--------------------------------------------------------------------------------
isFull :: Room -> Bool
isFull room =
  occupancy room >= roomCapacity room


--------------------------------------------------------------------------------
occupancy :: Room -> Int
occupancy = length . roomPlayers
