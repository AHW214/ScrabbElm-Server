--------------------------------------------------------------------------------
------------ save me from the cyclic dependencies which ive become -------------
--------------------------------------------------------------------------------

module Scrabble.Types where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (TBQueue)
import           Data.Map               (Map)
import           Data.Text              (Text)
import           Network.WebSockets     (Connection)


--------------------------------------------------------------------------------
------------------------------------ models ------------------------------------
--------------------------------------------------------------------------------
data Lobby = Lobby
  { lobbyClientQueues :: Map Text Client
  , lobbyQueue        :: TBQueue LobbyMessage
  , lobbyRoomViews    :: Map Text RoomView
  }


--------------------------------------------------------------------------------
data Room = Room
  { roomCapacity :: Int
  , roomName     :: Text
  , roomOwner    :: Player
  , roomPlayers  :: Map Client Player
  , roomPlaying  :: Maybe Player
  , roomQueue    :: TBQueue RoomMessage
  }


--------------------------------------------------------------------------------
data RoomView = RoomView
  { roomViewCapacity  :: Int
  , roomViewInGame    :: Bool
  , roomViewName      :: Text
  , roomViewOccupancy :: Int
  , roomViewQueue     :: TBQueue RoomMessage
  }


--------------------------------------------------------------------------------
data Player = Player
  { playerName   :: Text
  , playerScore  :: Int
  }


--------------------------------------------------------------------------------
data Client = Client
  { clientConnection :: Connection
  , clientId         :: Text
  , clientQueue      :: TBQueue ClientMessage
  }


--------------------------------------------------------------------------------
------------------------------------ events ------------------------------------
--------------------------------------------------------------------------------
type LobbyMessage =
  Message LobbyEventInternal LobbyEventExternal


--------------------------------------------------------------------------------
data LobbyEventInternal
  = LobbyClientJoin  Client
  | LobbyClientLeave Client
  | LobbyRoomRun     Room
  | LobbyRoomUpdate  Room
  | LobbyRoomRemove  Room


--------------------------------------------------------------------------------
data LobbyEventExternal
  = LobbyRoomMake RoomMake
  | LobbyRoomJoin RoomJoin


--------------------------------------------------------------------------------
type RoomMessage =
  Message RoomEventInternal RoomEventExternal


--------------------------------------------------------------------------------
data RoomEventInternal
  = RoomOwnerJoin Client Text


--------------------------------------------------------------------------------
data RoomEventExternal
  = RoomPlayerJoin RoomJoin
  | RoomPlayerLeave


--------------------------------------------------------------------------------
type ClientMessage =
  Message ClientEventInternal ClientEventExternal


--------------------------------------------------------------------------------
data ClientEventInternal
  = ClientRoomJoin Room
  | ClientRoomLeave
  | ClientDisconnect


--------------------------------------------------------------------------------
data ClientEventExternal
  = ClientMessageSend Text


--------------------------------------------------------------------------------
data Message a b
  = MessageInternal a
  | MessageExternal Client b


--------------------------------------------------------------------------------
------------------------------- external messages ------------------------------
--------------------------------------------------------------------------------
data RoomMake = RM
  { rmRoomCapacity :: Int
  , rmPlayerName   :: Text
  , rmRoomName     :: Text
  }


--------------------------------------------------------------------------------
data RoomJoin = RJ
  { rjPlayerName :: Text
  , rjRoomName   :: Text
  }
