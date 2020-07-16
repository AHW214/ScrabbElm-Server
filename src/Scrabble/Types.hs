--------------------------------------------------------------------------------
------------ save me from the cyclic dependencies which ive become -------------
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

module Scrabble.Types where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (TBQueue)
import           Data.Aeson             (FromJSON, ToJSON (..), ToJSONKey)
import           Data.Map               (Map)
import           Data.Set               (Set)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Network.WebSockets     (Connection)
import           Text.Read              (readMaybe)

import qualified Data.Aeson             as JSON
import qualified Data.Text              as Text


--------------------------------------------------------------------------------
------------------------------------ server ------------------------------------
--------------------------------------------------------------------------------
data Context = Context
  { contextLobbyQueue  :: LobbyQueue
  , contextLoggerQueue :: LoggerQueue
  , contextLogLevel    :: LogLevel
  }


--------------------------------------------------------------------------------
data Log =
  Log LogLevel Text


--------------------------------------------------------------------------------
data LogLevel
  = LogError
  | LogWarning
  | LogInfo
  | LogDebug
  deriving (Eq, Ord, Read)


--------------------------------------------------------------------------------
instance FromJSON LogLevel where
  parseJSON = JSON.withText "LogLevel" $ \text ->
    case fromText text of
      Just logLevel ->
        pure logLevel

      _ ->
        fail "invalid LogLevel"
    where
      fromText :: Text -> Maybe LogLevel
      fromText = readMaybe . Text.unpack . Text.toTitle


--------------------------------------------------------------------------------
------------------------------------ models ------------------------------------
--------------------------------------------------------------------------------
data Lobby = Lobby
  { lobbyClients :: Map Text Client
  , lobbyQueue   :: LobbyQueue
  , lobbyRooms   :: Map Text ( RoomView, RoomQueue )
  }


--------------------------------------------------------------------------------
data Room = Room
  { roomCapacity       :: Int
  , roomName           :: Text
  , roomOwner          :: Client
  , roomPendingClients :: Set Client
  , roomPlayers        :: Map Client Player
  , roomPlaying        :: Maybe Player
  } deriving Generic


--------------------------------------------------------------------------------
instance ToJSON Room where


--------------------------------------------------------------------------------
data RoomView = RoomView
  { roomViewCapacity  :: Int
  , roomViewInGame    :: Bool
  , roomViewName      :: Text
  , roomViewOccupancy :: Int
  } deriving Generic


--------------------------------------------------------------------------------
instance ToJSON RoomView where


--------------------------------------------------------------------------------
data Player = Player
  { playerName  :: Text
  , playerScore :: Int
  } deriving Generic


--------------------------------------------------------------------------------
instance ToJSON Player where


--------------------------------------------------------------------------------
instance Eq Player where
  Player { playerName = name1 } == Player { playerName = name2 } =
    name1 == name2


--------------------------------------------------------------------------------
data Client = Client
  { clientConnection :: Connection
  , clientId         :: Text
  , clientQueue      :: ClientQueue
  }


--------------------------------------------------------------------------------
instance ToJSONKey Client where


--------------------------------------------------------------------------------
instance ToJSON Client where
  toJSON = toJSON . clientId

  toEncoding = toEncoding . clientId


--------------------------------------------------------------------------------
instance Ord Client where
  compare Client { clientId = id1 } Client { clientId = id2 } =
    compare id1 id2


--------------------------------------------------------------------------------
instance Eq Client where
 Client { clientId = id1 } == Client { clientId = id2 } =
   id1 == id2


--------------------------------------------------------------------------------
------------------------------------ events ------------------------------------
--------------------------------------------------------------------------------
type LoggerQueue =
  TBQueue Log


--------------------------------------------------------------------------------
type LobbyQueue =
  TBQueue LobbyEvent


--------------------------------------------------------------------------------
type LobbyEvent =
  Event LobbyEventInternal LobbyEventExternal


--------------------------------------------------------------------------------
data LobbyEventInternal
  = LobbyClientJoin  Client
  | LobbyClientLeave Client
  | LobbyRoomRun     Room RoomQueue
  | LobbyRoomUpdate  Room
  | LobbyRoomRemove  Room


--------------------------------------------------------------------------------
data LobbyEventExternal
  = LobbyRoomMake RoomMake
  | LobbyRoomJoin RoomJoin


--------------------------------------------------------------------------------
type RoomQueue =
  TBQueue RoomEvent


--------------------------------------------------------------------------------
type RoomEvent =
  Event RoomEventInternal RoomEventExternal


--------------------------------------------------------------------------------
data RoomEventInternal
  = RoomPlayerJoin  Client RoomQueue
  | RoomPlayerLeave Client


--------------------------------------------------------------------------------
newtype RoomEventExternal
  = RoomPlayerSetName PlayerSetName


--------------------------------------------------------------------------------
type ClientQueue =
  TBQueue ClientEvent


--------------------------------------------------------------------------------
type ClientEvent =
  Event ClientEventInternal ClientEventExternal


--------------------------------------------------------------------------------
data ClientEventInternal
  = ClientMessageSend MessageOutbound
  | ClientRoomJoin    Room RoomQueue
  | ClientRoomLeave
  | ClientDisconnect


--------------------------------------------------------------------------------
newtype ClientEventExternal
  = ClientMessageReceive (Either Text MessageInbound)


--------------------------------------------------------------------------------
data Event a b
  = EventInternal a
  | EventExternal Client b


--------------------------------------------------------------------------------
------------------------------- inbound messages -------------------------------
--------------------------------------------------------------------------------
data MessageInbound
  = RoomMakeInbound      RoomMake
  | RoomJoinInbound      RoomJoin
  | RoomLeaveInbound
  | PlayerSetNameInbound PlayerSetName
  deriving Generic


--------------------------------------------------------------------------------
instance FromJSON MessageInbound


--------------------------------------------------------------------------------
data RoomMake = RM
  { rmRoomCapacity :: Int
  , rmRoomName     :: Text
  } deriving Generic


--------------------------------------------------------------------------------
instance FromJSON RoomMake


--------------------------------------------------------------------------------
data RoomJoin = RJ
  { rjRoomName :: Text
  } deriving Generic


--------------------------------------------------------------------------------
instance FromJSON RoomJoin


--------------------------------------------------------------------------------
data PlayerSetName = PSN
  { psnPlayerName :: Text
  } deriving Generic


--------------------------------------------------------------------------------
instance FromJSON PlayerSetName


--------------------------------------------------------------------------------
------------------------------- outbound messages ------------------------------
--------------------------------------------------------------------------------
data MessageOutbound
  = RoomListOutbound           [ RoomView ]
  | RoomUpdateOutbound         RoomView
  | RoomRemoveOutbound         Text
  | RoomJoinOutbound           Room
  | RoomLeaveOutbound
  | PlayerJoinOutbound         Text
  | PlayerLeaveOutbound        Text
  | PendingClientLeaveOutbound
  | ErrorOutbound              Error
  deriving Generic


--------------------------------------------------------------------------------
instance ToJSON MessageOutbound where


--------------------------------------------------------------------------------
------------------------------------ errors ------------------------------------
--------------------------------------------------------------------------------
data Error
  = MessageInvalid Text
  | ClientNotInLobby
  | ClientNotInRoom
  | RoomCapacityInvalid
  | RoomAlreadyExists
  | RoomDoesNotExist
  | RoomIsFull
  | RoomInGame
  | RoomHasPlayer
  | PlayerNameAlreadySet
  deriving Generic


--------------------------------------------------------------------------------
instance ToJSON Error where
