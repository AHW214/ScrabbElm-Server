module Scrabble.Lobby
  ( Lobby (..)
  , LobbyEvent (..)
  , addClient
  , addRoomView
  , getRoomView
  , removeClient
  , removeRoomView
  , updateRoomView
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (TBQueue)
import           Data.Map               (Map)
import           Data.Text              (Text)

import           Scrabble.Client        (Client (..), ClientEvent)
import           Scrabble.Room          (Room (..), RoomView (..))

import qualified Data.Map               as Map

import qualified Scrabble.Room          as Room


--------------------------------------------------------------------------------
data Lobby = Lobby
  { lobbyClientQueues :: Map Text (TBQueue ClientEvent)
  , lobbyQueue        :: TBQueue LobbyEvent
  , lobbyRoomViews    :: Map Text RoomView
  }


--------------------------------------------------------------------------------
data LobbyEvent
  = LobbyClientJoin Client
  | LobbyClientLeave Client
  | LobbyRoomMake Text Int Text Client
  | LobbyRoomRun Room
  | LobbyRoomJoin Text Text Client
  | LobbyRoomUpdate Room
  | LobbyRoomRemove Room


--------------------------------------------------------------------------------
getRoomView :: Text -> Lobby -> Maybe RoomView
getRoomView roomName = Map.lookup roomName . lobbyRoomViews


--------------------------------------------------------------------------------
removeRoomView :: Room -> Lobby -> Lobby
removeRoomView Room { roomName } lobby@Lobby { lobbyRoomViews } =
  lobby
    { lobbyRoomViews =
        Map.delete roomName lobbyRoomViews
    }


--------------------------------------------------------------------------------
updateRoomView :: Room -> Lobby -> Lobby
updateRoomView = addRoomView


--------------------------------------------------------------------------------
addRoomView :: Room -> Lobby -> Lobby
addRoomView room lobby@Lobby { lobbyRoomViews } =
  lobby
    { lobbyRoomViews =
        Map.insert roomViewName roomView lobbyRoomViews
    }
  where
    roomView :: RoomView
    roomView@RoomView { roomViewName } =
      Room.toView room


--------------------------------------------------------------------------------
removeClient :: Client -> Lobby -> Lobby
removeClient Client { clientId } lobby@Lobby { lobbyClientQueues } =
  lobby
    { lobbyClientQueues =
        Map.delete clientId lobbyClientQueues
    }


--------------------------------------------------------------------------------
addClient :: Client -> Lobby -> Lobby
addClient Client { clientQueue, clientId } lobby@Lobby { lobbyClientQueues } =
  lobby
    { lobbyClientQueues =
        Map.insert clientId clientQueue lobbyClientQueues
    }
