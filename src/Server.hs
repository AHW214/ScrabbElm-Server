module Server
  ( Server (..)
  , addClient
  , addPendingTicket
  , addRoom
  , clientExists
  , getRoom
  , isPendingTicket
  , new
  , removeClient
  , removePendingTicket
  , removeRoom
  ) where


--------------------------------------------------------------------------------
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Text          (Text)
import           Network.WebSockets (Connection)


--------------------------------------------------------------------------------
import           Room    (Room (..))
import           Tickets (Ticket)


--------------------------------------------------------------------------------
data Server
  = Server
      { pendingTickets :: Set Ticket
      , clients        :: Map Ticket Connection
      , rooms          :: Map Text Room
      }


--------------------------------------------------------------------------------
new :: Server
new =
  Server
    { pendingTickets = Set.empty
    , clients        = Map.empty
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
addClient :: Ticket -> Connection -> Server -> Server
addClient ticket conn server =
  server { clients = Map.insert ticket conn $ clients server }


--------------------------------------------------------------------------------
removeClient :: Ticket -> Server -> Server
removeClient ticket server =
  server { clients = Map.delete ticket $ clients server }


--------------------------------------------------------------------------------
clientExists :: Ticket -> Server -> Bool
clientExists ticket =
  Map.member ticket . clients


--------------------------------------------------------------------------------
addRoom :: Room -> Server -> Server
addRoom room@Room { name } server@Server { rooms } =
  server { rooms = Map.insert name room rooms }


--------------------------------------------------------------------------------
removeRoom :: Text -> Server -> Server
removeRoom roomName server@Server { rooms } =
  server { rooms = Map.delete roomName rooms }


--------------------------------------------------------------------------------
getRoom :: Text -> Server -> Maybe Room
getRoom name = Map.lookup name . rooms
