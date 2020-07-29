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
import           Control.Concurrent.STM (STM, TVar)
import           Data.Map               (Map)
import           Data.Text              (Text)

-- import           Scrabble.Common     (ID)
import           Scrabble.Client        (Client (..))
import           Scrabble.Player        (Player)

import qualified Control.Concurrent.STM as STM
import qualified Data.Map               as Map

import qualified Scrabble.Player        as Player


--------------------------------------------------------------------------------
data Room = Room
  { roomCapacity :: Int
  , roomId       :: Text
  , roomPlayers  :: TVar (Map Text Player)
  }


--------------------------------------------------------------------------------
new :: Int -> Text -> STM Room
new roomCapacity =
  flip fmap (STM.newTVar Map.empty) . Room roomCapacity


--------------------------------------------------------------------------------
addPlayer :: Client -> Text -> Room -> STM ()
addPlayer Client { clientId } playerId Room { roomPlayers } =
  STM.modifyTVar' roomPlayers $ Map.insert clientId player
  where
    player :: Player
    player = Player.new playerId


--------------------------------------------------------------------------------
removePlayer :: Client -> Room -> STM ()
removePlayer Client { clientId } Room { roomPlayers } =
  STM.modifyTVar' roomPlayers $ Map.delete clientId


--------------------------------------------------------------------------------
isEmpty :: Room -> STM Bool
isEmpty =
  fmap null . STM.readTVar . roomPlayers


--------------------------------------------------------------------------------
isFull :: Room -> STM Bool
isFull room =
  (roomCapacity room <=) <$> occupancy room


--------------------------------------------------------------------------------
occupancy :: Room -> STM Int
occupancy = fmap length . getPlayers


--------------------------------------------------------------------------------
getPlayers :: Room -> STM (Map Text Player)
getPlayers = STM.readTVar . roomPlayers
