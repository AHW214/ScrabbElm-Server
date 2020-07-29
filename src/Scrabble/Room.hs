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
import           Data.Aeson          (ToJSON (toEncoding, toJSON), (.=))
import           Data.Map            (Map)
import           Data.Text           (Text)

-- import           Scrabble.Common     (ID)
import           Scrabble.Client     (Client (..))
import           Scrabble.Player     (Player)

import qualified Data.Aeson          as JSON
import qualified Data.Map            as Map

import qualified Scrabble.Player     as Player


--------------------------------------------------------------------------------
data Room = Room
  { roomCapacity :: Int
  , roomId       :: Text
  , roomPlayers  :: Map Text Player
  }


--------------------------------------------------------------------------------
instance ToJSON Room where
  toJSON Room { roomCapacity, roomId, roomPlayers } =
    JSON.object
      [ "roomCapacity"  .= roomCapacity
      , "roomId"        .= roomId
      , "roomOccupancy" .= length roomPlayers
      ]

  toEncoding Room { roomCapacity, roomId, roomPlayers } =
    JSON.pairs
      $  "roomCapacity"  .= roomCapacity
      <> "roomId"        .= roomId
      <> "roomOccupancy" .= length roomPlayers


--------------------------------------------------------------------------------
new :: Text -> Int -> Room
new roomId roomCapacity = Room
  { roomCapacity
  , roomId
  , roomPlayers = Map.empty
  }


--------------------------------------------------------------------------------
addPlayer :: Client -> Text -> Room -> Room
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
