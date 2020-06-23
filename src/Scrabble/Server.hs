module Scrabble.Server
  ( Server (..)
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
  , removeRoom
  ) where


--------------------------------------------------------------------------------
import           Data.Map.Strict         (Map)
import           Data.Text               (Text)
import           Network.WebSockets      (Connection)
import           TextShow                (showt)

import           Scrabble.Room           (Room (..))

import qualified Data.Map.Strict         as Map


--------------------------------------------------------------------------------
data Server = Server
  { serverClientCounter    :: Int
  , serverConnectedClients :: Map Text Connection
  , serverDirectory        :: Map Text Text
  , serverPendingClients   :: Map Text Text
  , serverRooms            :: Map Text Room
  }


--------------------------------------------------------------------------------
new :: Server
new = Server
  { serverClientCounter    = 0
  , serverConnectedClients = Map.empty
  , serverDirectory        = Map.empty
  , serverPendingClients   = Map.empty
  , serverRooms            = Map.empty
  }


--------------------------------------------------------------------------------
createPendingClient :: Text -> Server -> ( Server, Text )
createPendingClient ticket server@Server { serverClientCounter, serverPendingClients } =
  let
    clientId = "client-" <> showt serverClientCounter
  in
    ( server
        { serverClientCounter = serverClientCounter + 1
        , serverPendingClients = Map.insert clientId ticket serverPendingClients
        }
    , clientId
    )


--------------------------------------------------------------------------------
getPendingClient :: Text -> Server -> Maybe Text
getPendingClient clientId Server { serverPendingClients } =
  Map.lookup clientId serverPendingClients


--------------------------------------------------------------------------------
acceptPendingClient :: Text -> Connection -> Server -> Server
acceptPendingClient clientId clientConn server@Server { serverPendingClients, serverConnectedClients } =
  server
    { serverConnectedClients = Map.insert clientId clientConn serverConnectedClients
    , serverPendingClients = Map.delete clientId serverPendingClients
    }


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
getClientRoom :: Text -> Server -> Maybe Room
getClientRoom clientId Server { serverDirectory, serverRooms } =
  Map.lookup clientId serverDirectory >>= flip Map.lookup serverRooms


--------------------------------------------------------------------------------
joinRoom :: Text -> Text -> Server -> Server
joinRoom clientId roomName server@Server { serverDirectory } =
  server { serverDirectory = Map.insert clientId roomName serverDirectory }


--------------------------------------------------------------------------------
leaveRoom :: Text -> Server -> Server
leaveRoom clientId server@Server { serverDirectory } =
  server { serverDirectory = Map.delete clientId serverDirectory }


--------------------------------------------------------------------------------
inRoom :: Text -> Server -> Bool
inRoom clientId =
  Map.member clientId . serverDirectory
