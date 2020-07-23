module Scrabble.Lobby
  ( Lobby (..)
  , addClient
  , addRoom
  , getRoom
  , listRoomViews
  , new
  , removeClient
  , removeRoom
  , updateRoomView
  ) where


--------------------------------------------------------------------------------
import           Data.Text       (Text)

import           Scrabble.Client (Client (..))
import           Scrabble.Room   (Room (..), RoomView (..))
import           Scrabble.Types  (EventQueue, Lobby (..))

import qualified Data.Map        as Map

import qualified Scrabble.Room   as Room


--------------------------------------------------------------------------------
listRoomViews :: Lobby -> [ RoomView ]
listRoomViews = fmap fst . Map.elems . lobbyRooms


--------------------------------------------------------------------------------
getRoom :: Text -> Lobby -> Maybe ( RoomView, EventQueue Room )
getRoom name = Map.lookup name . lobbyRooms


--------------------------------------------------------------------------------
removeRoom :: Room -> Lobby -> Lobby
removeRoom Room { roomName } lobby@Lobby { lobbyRooms } = lobby
  { lobbyRooms =
      Map.delete roomName lobbyRooms
  }


--------------------------------------------------------------------------------
updateRoomView :: RoomView -> Lobby -> Lobby
updateRoomView roomView@RoomView { roomViewName } lobby@Lobby { lobbyRooms } = lobby
  { lobbyRooms =
      Map.adjust (\( _, rq ) -> ( roomView, rq )) roomViewName lobbyRooms
  }


--------------------------------------------------------------------------------
addRoom :: EventQueue Room -> Room -> Lobby -> Lobby
addRoom roomQueue room lobby@Lobby { lobbyRooms } = lobby
  { lobbyRooms =
      Map.insert roomViewName ( roomView, roomQueue ) lobbyRooms
  }
  where
    roomView :: RoomView
    roomView@RoomView { roomViewName } =
      Room.toView room


--------------------------------------------------------------------------------
removeClient :: Client -> Lobby -> Lobby
removeClient Client { clientId } lobby@Lobby { lobbyClients } = lobby
  { lobbyClients =
      Map.delete clientId lobbyClients
  }


--------------------------------------------------------------------------------
addClient :: Client -> Lobby -> Lobby
addClient client@Client { clientId } lobby@Lobby { lobbyClients } = lobby
  { lobbyClients =
      Map.insert clientId client lobbyClients
  }


--------------------------------------------------------------------------------
new :: EventQueue Lobby -> Lobby
new eventQueue = Lobby
  { lobbyClients = Map.empty
  , lobbyQueue   = eventQueue
  , lobbyRooms   = Map.empty
  }
