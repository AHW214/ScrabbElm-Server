module Scrabble.Server
  ( Server (..)
  , PendingParams (..)
  , acceptPendingClient
  , addRoom
  , clientExists
  , clientsInLobby
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
import           Data.Map.Strict         (Map)
import           Data.Text               (Text)
import           Data.Time.Clock         (NominalDiffTime)
import           Network.WebSockets      (Connection)
import           TextShow                (showt)

import           Scrabble.Authentication (Secret)
import           Scrabble.Client         (Client (..))
import           Scrabble.Config         (Config (..))
import           Scrabble.Log.Level      (LogLevel (..))
import           Scrabble.Room           (Room (..))

import qualified Data.Map.Strict         as Map

import qualified Scrabble.Client         as Client
import qualified Scrabble.Room           as Room


--------------------------------------------------------------------------------
data Server = Server
  { serverAuthSecret       :: Secret
  , serverClientCounter    :: Int
  , serverConnectedClients :: Map Text Client
  , serverLogLevel         :: LogLevel
  , serverRoomDirectory    :: Map Client Text
  , serverPendingClients   :: Map Text Text
  , serverPendingTimeout   :: NominalDiffTime
  , serverRooms            :: Map Text Room
  }


--------------------------------------------------------------------------------
data PendingParams = PendingParams
  { pendingAuthSecret :: Secret
  , pendingClientId   :: Text
  , pendingTimeout    :: NominalDiffTime
  }


--------------------------------------------------------------------------------
new :: Config -> Server
new Config { configAuthSecret, configLogLevel, configPendingTimeout } =
  Server
    { serverAuthSecret       = configAuthSecret
    , serverClientCounter    = 0
    , serverConnectedClients = Map.empty
    , serverLogLevel         = configLogLevel
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
    { serverAuthSecret
    , serverClientCounter
    , serverPendingClients
    , serverPendingTimeout
    }
  = ( server
        { serverClientCounter = serverClientCounter + 1
        , serverPendingClients = Map.insert clientId ticket serverPendingClients
        }
    , PendingParams
        { pendingAuthSecret = serverAuthSecret
        , pendingClientId   = clientId
        , pendingTimeout    = serverPendingTimeout
        }
    )
    where
      clientId = "client-" <> showt serverClientCounter


--------------------------------------------------------------------------------
getPendingClient :: Text -> Server -> Maybe Text
getPendingClient clientId Server { serverPendingClients } =
  Map.lookup clientId serverPendingClients


--------------------------------------------------------------------------------
acceptPendingClient :: Text -> Connection -> Server -> ( Server, Client )
acceptPendingClient clientId clientConn server@Server { serverPendingClients, serverConnectedClients } =
  ( server
      { serverConnectedClients = Map.insert clientId client serverConnectedClients
      , serverPendingClients = Map.delete clientId serverPendingClients
      }
  , client
  )
  where
    client :: Client
    client = Client.new clientConn clientId


--------------------------------------------------------------------------------
removePendingClient :: Text -> Server -> Server
removePendingClient clientId server@Server { serverPendingClients } =
  server { serverPendingClients = Map.delete clientId serverPendingClients }


--------------------------------------------------------------------------------
removeConnectedClient :: Client -> Server -> Server
removeConnectedClient
  client@Client
    { clientId
    }
  server@Server
    { serverConnectedClients
    , serverRoomDirectory
    , serverRooms
    }
  = case Map.lookup client serverRoomDirectory of
      Nothing ->
        server

      Just name ->
        server
          { serverConnectedClients = Map.delete clientId serverConnectedClients
          , serverRoomDirectory = Map.delete client serverRoomDirectory
          , serverRooms = Map.update (Room.removeClient client) name serverRooms
          }


--------------------------------------------------------------------------------
clientExists :: Text -> Server -> Bool
clientExists clientId =
  Map.member clientId . serverConnectedClients


--------------------------------------------------------------------------------
clientsInLobby :: Server -> Map Text Client
clientsInLobby server =
  clientsWho (flip inLobby server) server


--------------------------------------------------------------------------------
clientsWho :: (Client -> Bool) -> Server -> Map Text Client
clientsWho predicate =
  Map.filter predicate . serverConnectedClients


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
getClientRoom client Server { serverRoomDirectory, serverRooms } =
  Map.lookup client serverRoomDirectory >>= flip Map.lookup serverRooms


--------------------------------------------------------------------------------
joinRoom :: Client -> Text -> Server -> Server
joinRoom client roomName server@Server { serverRoomDirectory } =
  server { serverRoomDirectory = Map.insert client roomName serverRoomDirectory }


-------------------------------------------------------------------------------- TODO
leaveRoom :: Client -> Server -> Server
leaveRoom client server@Server { serverRoomDirectory } =
  server { serverRoomDirectory = Map.delete client serverRoomDirectory }


--------------------------------------------------------------------------------
inLobby :: Client -> Server -> Bool
inLobby client = not . inRoom client


--------------------------------------------------------------------------------
inRoom :: Client -> Server -> Bool
inRoom client =
  Map.member client . serverRoomDirectory
