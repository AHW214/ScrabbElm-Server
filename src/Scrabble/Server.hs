module Scrabble.Server
  ( Server (..)
  , acceptConnection
  , addPendingTicket
  , addRoom
  , clientsWho
  , connectionExists
  , getClientRoom
  , getRoom
  , inRoom
  , isPendingTicket
  , joinRoom
  , leaveRoom
  , new
  , removeConnection
  , removePendingTicket
  , removeRoom
  ) where


--------------------------------------------------------------------------------
import           Data.Map.Strict         (Map)
import           Data.Set                (Set)
import           Data.Text               (Text)
import           Network.WebSockets      (Connection)
import           TextShow                (showt)

import           Scrabble.Room           (Room (..))

import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set

import           Scrabble.Authentication as Auth


--------------------------------------------------------------------------------
data Server = Server
  { serverClientCounter  :: Int
  , serverClients        :: Map Text Connection
  , serverDirectory      :: Map Text Text
  , serverPendingClients :: Map Text Auth.Plain
  , serverRooms          :: Map Text Room
  }


--------------------------------------------------------------------------------
new :: Server
new = Server
  { serverClientCounter  = 0
  , serverClients        = Map.empty
  , serverDirectory      = Map.empty
  , serverPendingClients = Map.empty
  , serverRooms          = Map.empty
  }


--------------------------------------------------------------------------------
createPendingClient :: Auth.Plain -> Server -> Server
createPendingClient ticket server@Server { serverClientCounter, serverPendingClients } =
  let
    clientId = "client-" <> showt serverClientCounter
  in
    server
      { serverClientCounter = serverClientCounter + 1
      , serverPendingClients = Map.insert clientId ticket servePendingClients
      }


--------------------------------------------------------------------------------
acceptConnection :: Ticket -> Connection -> Server -> Server
acceptConnection ticket conn server@Server { serverConnections } =
  server { serverConnections = Map.insert ticket conn $ serverConnections }


--------------------------------------------------------------------------------
removeConnection :: Ticket -> Server -> Server
removeConnection ticket server@Server { serverConnections } =
  server { serverConnections = Map.delete ticket $ serverConnections }


--------------------------------------------------------------------------------
connectionExists :: Ticket -> Server -> Bool
connectionExists ticket =
  Map.member ticket . serverConnections


--------------------------------------------------------------------------------
clientsWho :: (Ticket -> Bool) -> Server -> Map Ticket Connection
clientsWho predicate =
  Map.filterWithKey (\k _ -> predicate k) . serverConnections


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
getClientRoom :: Ticket -> Server -> Maybe Room
getClientRoom ticket Server { serverDirectory, serverRooms } =
  Map.lookup ticket serverDirectory >>= flip Map.lookup serverRooms


--------------------------------------------------------------------------------
joinRoom :: Ticket -> Text -> Server -> Server
joinRoom ticket roomName server@Server { serverDirectory } =
  server { serverDirectory = Map.insert ticket roomName serverDirectory }


--------------------------------------------------------------------------------
leaveRoom :: Ticket -> Server -> Server
leaveRoom ticket server@Server { serverDirectory } =
  server { serverDirectory = Map.delete ticket serverDirectory }


--------------------------------------------------------------------------------
inRoom :: Ticket -> Server -> Bool
inRoom ticket =
  Map.member ticket . serverDirectory
