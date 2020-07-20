--------------------------------------------------------------------------------
------------ save me from the cyclic dependencies which ive become -------------
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Scrabble.Types where


--------------------------------------------------------------------------------
import           Control.Concurrent.Async (Async)
import           Control.Concurrent.STM   (STM, TBQueue)
import           Data.Aeson               (FromJSON (..), ToJSON (..), ToJSONKey)
import           Data.Kind                (Type)
import           Data.Map.Strict          (Map)
import           Data.Set                 (Set)
import           Data.String              (IsString (..))
import           Data.Text                (Text)
import           Data.Time                (NominalDiffTime)
import           GHC.Generics             (Generic)
import           Network.WebSockets       (Connection)
import           Numeric.Natural          (Natural)
import           System.Random            (StdGen)
import           Text.Read                (readMaybe)
import           Web.JWT                  (Signer (..))

import qualified Control.Arrow            as Arrow
import qualified Control.Concurrent.STM   as STM
import qualified Data.Aeson               as JSON
import qualified Data.Foldable            as Foldable
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text
import qualified Network.WebSockets       as WS


--------------------------------------------------------------------------------
------------------------------------ server ------------------------------------
--------------------------------------------------------------------------------
data Context = Context
  { contextLobbyQueue  :: EventQueue Lobby
  , contextLoggerQueue :: TBQueue Log
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
---------------------------------- concurrency ---------------------------------
--------------------------------------------------------------------------------
class Talk a where
  queueBound :: QueueBound a

  newQueue :: STM (TBQueue a)
  newQueue = STM.newTBQueue bound
    where
      (QueueBound bound) = (queueBound :: QueueBound a)

  emit :: TBQueue a -> a -> STM ()
  emit = STM.writeTBQueue

  receive :: TBQueue a -> STM a
  receive = STM.readTBQueue

  newQueueIO :: IO (TBQueue a)
  newQueueIO = STM.atomically newQueue

  emitIO :: TBQueue a -> a -> IO ()
  emitIO queue = STM.atomically . emit queue

  receiveIO :: TBQueue a -> IO a
  receiveIO = STM.atomically . receive


--------------------------------------------------------------------------------
instance Model a => Talk (Event a) where
  queueBound = modelQueueBound


--------------------------------------------------------------------------------
instance Talk Log where
  queueBound = 4096


--------------------------------------------------------------------------------
newtype QueueBound a = QueueBound Natural
  deriving Num


--------------------------------------------------------------------------------
------------------------------------ models ------------------------------------
--------------------------------------------------------------------------------
class Model a where
  data Event a :: Type

  modelQueueBound :: QueueBound (Event a)
  modelQueueBound = 256


--------------------------------------------------------------------------------
type EventQueue a = TBQueue (Event a)


--------------------------------------------------------------------------------
data Gateway = Gateway
  { gatewayAuthSecret    :: Secret
  , gatewayQueue         :: EventQueue Gateway
  , gatewayStdGen        :: StdGen
  , gatewayTimeoutLength :: NominalDiffTime
  , gatewayTimeouts      :: Map Text (Async ())
  }


--------------------------------------------------------------------------------
instance Model Gateway where
  data Event Gateway
    = GatewayCreateJWT     (TBQueue Text)
    | GatewayAddTimeout    Text (Async ())
    | GatewayDeleteTimeout Text
    | GatewayAuthenticate  Text Connection (TBQueue (Either Error (EventQueue Client)))

  modelQueueBound = 4096


--------------------------------------------------------------------------------
newtype Secret = Secret Signer


--------------------------------------------------------------------------------
instance IsString Secret where
  fromString = Secret . HMACSecret . fromString


--------------------------------------------------------------------------------
instance FromJSON Secret where
  parseJSON = fmap fromString . parseJSON


--------------------------------------------------------------------------------
data Lobby = Lobby
  { lobbyClients :: Map Text Client
  , lobbyQueue   :: EventQueue Lobby
  , lobbyRooms   :: Map Text ( RoomView, (EventQueue Room) )
  }


--------------------------------------------------------------------------------
instance Model Lobby where
  data Event Lobby
    = LobbyClientJoin  Client
    | LobbyClientLeave Client
    | LobbyRoomMake    RoomMake Client
    | LobbyRoomJoin    RoomJoin Client
    | LobbyRoomRun     Room (EventQueue Room)
    | LobbyRoomUpdate  Room
    | LobbyRoomRemove  Room

  modelQueueBound = 4096


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
instance Model Room where
  data Event Room
    = RoomPlayerJoin    Client (EventQueue Room)
    | RoomPlayerLeave   Client
    | RoomPlayerSetName PlayerSetName Client


--------------------------------------------------------------------------------
instance ToJSON Room


--------------------------------------------------------------------------------
data RoomView = RoomView
  { roomViewCapacity  :: Int
  , roomViewInGame    :: Bool
  , roomViewName      :: Text
  , roomViewOccupancy :: Int
  } deriving Generic


--------------------------------------------------------------------------------
instance ToJSON RoomView


--------------------------------------------------------------------------------
data Player = Player
  { playerName  :: Text
  , playerScore :: Int
  } deriving Generic


--------------------------------------------------------------------------------
instance ToJSON Player


--------------------------------------------------------------------------------
instance Eq Player where
  Player { playerName = name1 } == Player { playerName = name2 } =
    name1 == name2


--------------------------------------------------------------------------------
data Client = Client
  { clientConnection :: Connection
  , clientId         :: Text
  , clientQueue      :: EventQueue Client
  , clientRoomQueue  :: Maybe (EventQueue Room)
  }


--------------------------------------------------------------------------------
instance Model Client where
  data Event Client
    = ClientMessageReceive (Either Text (Message Inbound))
    | ClientMessageSend    (Message Outbound)
    | ClientRoomJoin       Room (EventQueue Room)
    | ClientRoomLeave
    | ClientDisconnect


--------------------------------------------------------------------------------
instance ToJSONKey Client


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
-------------------------------- communications --------------------------------
--------------------------------------------------------------------------------
class Monad m => Communicate m where
  broadcastLobby :: Lobby -> Message Outbound -> m ()

  broadcastRoom :: Room -> Message Outbound -> m ()

  errorToClient :: Client -> Error -> m ()

  toClients :: Foldable t => t Client -> Message Outbound -> m ()

  toClient :: Client -> Message Outbound -> m ()

  closeConnection :: Connection -> Error -> m ()

  toConnection :: Connection -> Message Outbound -> m ()

  fromConnection :: Connection -> m (Either Text (Message Inbound))


--------------------------------------------------------------------------------
instance Communicate IO where
  broadcastLobby Lobby { lobbyClients } =
    toClients lobbyClients

  broadcastRoom =
    toClients . Map.keys . roomPlayers

  errorToClient client =
    toClient client . ErrorOutbound

  toClients clients message =
    Foldable.traverse_ (flip toClient message) clients -- perform all as STM atomically ?

  toClient Client { clientQueue } =
    emitIO clientQueue . ClientMessageSend

  closeConnection connection =
    WS.sendClose connection . JSON.encode . ErrorOutbound

  toConnection connection =
    WS.sendTextData connection . JSON.encode

  fromConnection =
    fmap (Arrow.left Text.pack . JSON.eitherDecodeStrict')
    . WS.receiveData


--------------------------------------------------------------------------------
class Communication a where
  data Message a :: Type


--------------------------------------------------------------------------------
data Inbound = Inbound


--------------------------------------------------------------------------------
instance Communication Inbound where
  data Message Inbound
    = RoomMakeInbound      RoomMake
    | RoomJoinInbound      RoomJoin
    | RoomLeaveInbound
    | PlayerSetNameInbound PlayerSetName
    deriving Generic


--------------------------------------------------------------------------------
instance FromJSON (Message Inbound)


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
data Outbound = Outbound


--------------------------------------------------------------------------------
instance Communication Outbound where
  data Message Outbound
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
instance ToJSON (Message Outbound)


--------------------------------------------------------------------------------
------------------------------------ errors ------------------------------------
--------------------------------------------------------------------------------
data Error
  = MessageInvalid Text
  | AuthFormatInvalid
  | AuthIdentityInvalid
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
instance ToJSON Error
