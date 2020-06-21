module Server
  ( Server (..)
  , acceptConnection
  , addPendingTicket
  , addRoom
  , connectionExists
  , getRoom
  , isPendingTicket
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
      { pendingTickets :: Set Ticket
      , connections    :: Map Ticket Connection
      , rooms          :: Map Text Room
      }


--------------------------------------------------------------------------------
new :: Server
new =
  Server
    { pendingTickets = Set.empty
    , connections    = Map.empty
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
