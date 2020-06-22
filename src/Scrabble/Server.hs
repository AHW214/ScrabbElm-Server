module Server
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
import qualified Data.Map.Strict    as Map
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import           Network.WebSockets (Connection)


--------------------------------------------------------------------------------
import           Room    (Room (Room))
import qualified Room
import           Tickets (Ticket)


--------------------------------------------------------------------------------
data Server
  = Server
      { connections    :: Map Ticket Connection
      , directory      :: Map Ticket Text
      , pendingTickets :: Set Ticket
      , rooms          :: Map Text Room
      }


--------------------------------------------------------------------------------
new :: Server
new =
  Server
    { connections    = Map.empty
    , directory      = Map.empty
    , pendingTickets = Set.empty
    , rooms          = Map.empty
    }


--------------------------------------------------------------------------------
addPendingTicket :: Ticket -> Server -> Server
addPendingTicket ticket server =
  server { pendingTickets = Set.insert ticket $ pendingTickets server }


--------------------------------------------------------------------------------
removePendingTicket :: Ticket -> Server -> Server
removePendingTicket ticket server =
  server { pendingTickets = Set.delete ticket $ pendingTickets server }


--------------------------------------------------------------------------------
isPendingTicket :: Ticket -> Server -> Bool
isPendingTicket ticket =
  Set.member ticket . pendingTickets


--------------------------------------------------------------------------------
acceptConnection :: Ticket -> Connection -> Server -> Server
acceptConnection ticket conn server =
  server { connections = Map.insert ticket conn $ connections server }


--------------------------------------------------------------------------------
removeConnection :: Ticket -> Server -> Server
removeConnection ticket server =
  server { connections = Map.delete ticket $ connections server }


--------------------------------------------------------------------------------
connectionExists :: Ticket -> Server -> Bool
connectionExists ticket =
  Map.member ticket . connections


--------------------------------------------------------------------------------
clientsWho :: (Ticket -> Bool) -> Server -> Map Ticket Connection
clientsWho predicate =
  Map.filterWithKey (\k _ -> predicate k) . connections


--------------------------------------------------------------------------------
addRoom :: Room -> Server -> Server
addRoom room@Room { Room.name } server@Server { rooms } =
  server { rooms = Map.insert name room rooms }


--------------------------------------------------------------------------------
removeRoom :: Room -> Server -> Server
removeRoom Room { Room.name } server@Server { rooms } =
  server { rooms = Map.delete name rooms }


--------------------------------------------------------------------------------
getRoom :: Text -> Server -> Maybe Room
getRoom name = Map.lookup name . rooms


--------------------------------------------------------------------------------
getClientRoom :: Ticket -> Server -> Maybe Room
getClientRoom ticket Server { directory, rooms } =
  Map.lookup ticket directory >>= flip Map.lookup rooms


--------------------------------------------------------------------------------
joinRoom :: Ticket -> Text -> Server -> Server
joinRoom ticket roomName server@Server { directory } =
  server { directory = Map.insert ticket roomName directory }


--------------------------------------------------------------------------------
leaveRoom :: Ticket -> Server -> Server
leaveRoom ticket server@Server { directory } =
  server { directory = Map.delete ticket directory }


--------------------------------------------------------------------------------
inRoom :: Ticket -> Server -> Bool
inRoom ticket Server { directory } =
  Map.member ticket directory
