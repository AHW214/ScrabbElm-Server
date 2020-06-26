module Scrabble.Server
  ( Server (..)
  , PendingParams (..)
  , acceptPendingClient
  , addRoom
  , clientExists
  , clientsWho
  , createPendingClient
  , getClientRoom
  , getPendingClient
  , getRoom
  , inRoom
  , joinRoom
  , leaveRoom
  , new
  , removeConnectedClient
  , removePendingClient
  , removeRoom
  ) where


--------------------------------------------------------------------------------
import           Data.ByteString    (ByteString)
import           Data.Map.Strict    (Map)
import           Data.Text          (Text)
import           Data.Time.Clock    (NominalDiffTime)
import           Network.WebSockets (Connection)
import           TextShow           (showt)

import           Scrabble.Client    (Client)
import           Scrabble.Config    (Config (..))
import           Scrabble.Room      (Room (..))

import qualified Data.Map.Strict    as Map
import qualified Data.Text.Encoding as Text

import qualified Scrabble.Client    as Client


--------------------------------------------------------------------------------
data Server = Server
  { serverAuthKey          :: ByteString
  , serverClientCounter    :: Int
  , serverConnectedClients :: Map Text Client
  , serverRoomDirectory    :: Map Client Text
  , serverPendingClients   :: Map Text Text
  , serverPendingTimeout   :: NominalDiffTime
  , serverRooms            :: Map Text Room
  }


--------------------------------------------------------------------------------
data PendingParams = PendingParams
  { pendingAuthKey  :: ByteString
  , pendingClientId :: Text
  , pendingTimeout  :: NominalDiffTime
  }


--------------------------------------------------------------------------------
new :: Config -> Server
new Config { configAuthKey, configPendingTimeout } =
  Server
    { serverAuthKey          = Text.encodeUtf8 configAuthKey
    , serverClientCounter    = 0
    , serverConnectedClients = Map.empty
    , serverRoomDirectory    = Map.empty
    , serverPendingClients   = Map.empty
    , serverPendingTimeout   = configPendingTimeout
    , serverRooms            = Map.empty
    }


--------------------------------------------------------------------------------
createPendingClient :: Text -> Server -> ( Server, PendingParams )
createPendingClient
  ticket
  server@Server
    { serverAuthKey
    , serverClientCounter
    , serverPendingClients
    , serverPendingTimeout
    }
  = let
      clientId = "client-" <> showt serverClientCounter
    in
      ( server
          { serverClientCounter = serverClientCounter + 1
          , serverPendingClients = Map.insert clientId ticket serverPendingClients
          }
      , PendingParams
          { pendingAuthKey  = serverAuthKey
          , pendingClientId = clientId
          , pendingTimeout  = serverPendingTimeout
          }
      )


--------------------------------------------------------------------------------
getPendingClient :: Text -> Server -> Maybe Text
getPendingClient clientId Server { serverPendingClients } =
  Map.lookup clientId serverPendingClients


--------------------------------------------------------------------------------
acceptPendingClient :: Text -> Connection -> Server -> Server
acceptPendingClient clientId clientConn server@Server { serverPendingClients, serverConnectedClients } =
  server
    { serverConnectedClients = Map.insert clientId client serverConnectedClients
    , serverPendingClients = Map.delete clientId serverPendingClients
    }
  where
    client :: Client
    client = Client.new clientConn clientId


--------------------------------------------------------------------------------
removePendingClient :: Text -> Server -> Server
removePendingClient clientId server@Server { serverPendingClients } =
  server { serverPendingClients = Map.delete clientId serverPendingClients }


--------------------------------------------------------------------------------
removeConnectedClient :: Text -> Server -> Server
removeConnectedClient clientId server@Server { serverConnectedClients } =
  server { serverConnectedClients = Map.delete clientId serverConnectedClients }


--------------------------------------------------------------------------------
clientExists :: Text -> Server -> Bool
clientExists clientId =
  Map.member clientId . serverConnectedClients


--------------------------------------------------------------------------------
clientsWho :: (Text -> Bool) -> Server -> Map Text Connection
clientsWho predicate =
  Map.filterWithKey (\k _ -> predicate k) . serverConnectedClients


--------------------------------------------------------------------------------
addRoom :: Room -> Server -> Server
addRoom room@Room { roomName } server@Server { serverRooms } =
  server { serverRooms = Map.insert roomName room serverRooms }


--------------------------------------------------------------------------------
removeRoom :: Room -> Server -> Server
removeRoom Room { roomName } server@Server { serverRooms } =
  server { serverRooms = Map.delete roomName serverRooms }


--------------------------------------------------------------------------------
getRoom :: Text -> Server -> Maybe Room
getRoom name = Map.lookup name . serverRooms


--------------------------------------------------------------------------------
getClientRoom :: Client -> Server -> Maybe Room
getClientRoom clientId Server { serverRoomDirectory, serverRooms } =
  Map.lookup clientId serverRoomDirectory >>= flip Map.lookup serverRooms


--------------------------------------------------------------------------------
joinRoom :: Client -> Text -> Server -> Server
joinRoom client roomName server@Server { serverRoomDirectory } =
  server { serverRoomDirectory = Map.insert client roomName serverRoomDirectory }


--------------------------------------------------------------------------------
leaveRoom :: Client -> Server -> Server
leaveRoom client server@Server { serverRoomDirectory } =
  server { serverRoomDirectory = Map.delete client serverRoomDirectory }


--------------------------------------------------------------------------------
inRoom :: Client -> Server -> Bool
inRoom client =
  Map.member client . serverRoomDirectory
