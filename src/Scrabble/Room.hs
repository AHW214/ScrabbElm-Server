module Scrabble.Room
  ( Room (..)
  , addPendingClient
  , addPlayer
  , broadcast
  , canJoin
  , getPlayer
  , hasPendingClient
  , hasPlayer
  , hasPlayerWithId
  , isEmpty
  , isFull
  , maxCapacity
  , new
  , occupancy
  , removePendingClient
  , removePlayer
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (STM, TChan, TVar)
import           Data.Map               (Map)
import           Data.Set               (Set)
import           Data.Text              (Text)

-- import           Scrabble.Common     (ID)
import           Scrabble.Client        (Client (..))
import           Scrabble.Message       (Message, Outbound)
import           Scrabble.Player        (Player)

import qualified Control.Concurrent.STM as STM
import qualified Data.List              as List
import qualified Data.Map               as Map
import qualified Data.Maybe             as Maybe
import qualified Data.Set               as Set

import qualified Scrabble.Player        as Player


--------------------------------------------------------------------------------
data Room = Room
  { roomBroadcastChan  :: TChan (Message Outbound)
  , roomCapacity       :: Int
  , roomId             :: Text
  , roomPendingClients :: TVar (Set Client)
  , roomPlayers        :: TVar (Map Text Player)
  , roomPlaying        :: TVar (Maybe Client)
  }


--------------------------------------------------------------------------------
maxCapacity :: Int
maxCapacity = 4


--------------------------------------------------------------------------------
new :: Int -> Text -> STM Room
new roomCapacity roomId = do
  roomBroadcastChan  <- STM.newBroadcastTChan
  roomPendingClients <- STM.newTVar Set.empty
  roomPlayers        <- STM.newTVar Map.empty
  roomPlaying        <- STM.newTVar Nothing

  pure Room
    { roomBroadcastChan
    , roomCapacity
    , roomId
    , roomPendingClients
    , roomPlayers
    , roomPlaying
    }


--------------------------------------------------------------------------------
broadcast :: Room -> Message Outbound -> STM ()
broadcast Room { roomBroadcastChan } =
  STM.writeTChan roomBroadcastChan


--------------------------------------------------------------------------------
removePlayer :: Client -> Room -> STM ()
removePlayer Client { clientId } Room { roomPlayers } =
  STM.modifyTVar' roomPlayers $ Map.delete clientId


--------------------------------------------------------------------------------
addPlayer :: Client -> Text -> Room -> STM ()
addPlayer Client { clientId } playerId Room { roomPlayers } =
  STM.modifyTVar' roomPlayers $ Map.insert clientId player
  where
    player :: Player
    player = Player.new playerId


--------------------------------------------------------------------------------
getPlayer :: Client -> Room -> STM (Maybe Player)
getPlayer Client { clientId } =
  fmap (Map.lookup clientId) . STM.readTVar . roomPlayers


--------------------------------------------------------------------------------
hasPlayerWithId :: Text -> Room -> STM Bool
hasPlayerWithId playerId =
  fmap (List.any (Player.hasId playerId) . Map.elems) . STM.readTVar . roomPlayers


--------------------------------------------------------------------------------
hasPlayer :: Client -> Room -> STM Bool
hasPlayer Client { clientId } =
  fmap (Map.member clientId) . STM.readTVar . roomPlayers


--------------------------------------------------------------------------------
removePendingClient :: Client -> Room -> STM ()
removePendingClient client Room { roomPendingClients } =
  STM.modifyTVar' roomPendingClients $ Set.delete client


--------------------------------------------------------------------------------
addPendingClient :: Client -> Room -> STM ()
addPendingClient client Room { roomPendingClients } =
  STM.modifyTVar' roomPendingClients $ Set.insert client


--------------------------------------------------------------------------------
hasPendingClient :: Client -> Room -> STM Bool
hasPendingClient client =
  fmap (Set.member client) . STM.readTVar . roomPendingClients


--------------------------------------------------------------------------------
canJoin :: Room -> STM Bool
canJoin room =
  not <$> ((||) <$> inGame room <*> isFull room) -- meh


--------------------------------------------------------------------------------
inGame :: Room -> STM Bool
inGame =
  fmap Maybe.isJust . STM.readTVar . roomPlaying


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
occupancy room = do
  pendingClients <- getPendingClients room
  players        <- getPlayers room
  pure $ length pendingClients + length players


--------------------------------------------------------------------------------
getPlayers :: Room -> STM (Map Text Player)
getPlayers = STM.readTVar . roomPlayers


--------------------------------------------------------------------------------
getPendingClients :: Room -> STM (Set Client)
getPendingClients = STM.readTVar . roomPendingClients
