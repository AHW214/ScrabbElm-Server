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
import           Data.Map.Strict    (Map)
import           Data.Set           (Set)
import           Data.Text          (Text)
import           Network.WebSockets (Connection)

import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set

import           Scrabble.Room      (Room (..))
import           Scrabble.Tickets   (Ticket)


--------------------------------------------------------------------------------
data Server = Server
  { serverConnections    :: Map Ticket Connection
  , serverDirectory      :: Map Ticket Text
  , serverPendingTickets :: Set Ticket
  , serverRooms          :: Map Text Room
  }


--------------------------------------------------------------------------------
new :: Server
new = Server
  { serverConnections    = Map.empty
  , serverDirectory      = Map.empty
  , serverPendingTickets = Set.empty
  , serverRooms          = Map.empty
  }


--------------------------------------------------------------------------------
addPendingTicket :: Ticket -> Server -> Server
addPendingTicket ticket server@Server { serverPendingTickets } =
  server { serverPendingTickets = Set.insert ticket $ serverPendingTickets }


--------------------------------------------------------------------------------
removePendingTicket :: Ticket -> Server -> Server
removePendingTicket ticket server@Server { serverPendingTickets } =
  server { serverPendingTickets = Set.delete ticket $ serverPendingTickets }


--------------------------------------------------------------------------------
isPendingTicket :: Ticket -> Server -> Bool
isPendingTicket ticket =
  Set.member ticket . serverPendingTickets


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
