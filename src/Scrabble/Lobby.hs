module Scrabble.Lobby
  (
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (TBQueue)
import           Data.Map               (Map)
import           Data.Text              (Text)

import           Scrabble.Client        (Client (..))
import           Scrabble.Room          (Room, RoomView (..))

import qualified Data.Map               as Map

import qualified Scrabble.Player        as Player
import qualified Scrabble.Room          as Room


--------------------------------------------------------------------------------
data Lobby = Lobby
  { lobbyClientQueues :: Map Text Text -- todo
  , lobbyQueue        :: TBQueue LobbyEvent
  , lobbyRoomViews    :: Map Text RoomView
  }


--------------------------------------------------------------------------------
data LobbyEvent
  = LobbyClientJoin Client
  | LobbyClientLeave Client
  | LobbyRoomMake Text Text Client
  | LobbyRoomJoin Text Text Client
  | LobbyRoomUpdate Room
  | LobbyRoomRemove Room


--------------------------------------------------------------------------------
eventHandler :: Lobby -> LobbyEvent -> ( Lobby, IO () )
eventHandler
  lobby@Lobby
    { lobbyClientQueues
    , lobbyQueue
    , lobbyRoomViews
    }
  = \case
      LobbyClientJoin client ->
        ( addClient client lobby
        , pure ()
        )

      LobbyClientLeave client ->
        ( removeClient client lobby
        , pure ()
        )

      LobbyRoomMake roomName playerName roomOwner ->
        let
          player = Player.new playerName roomOwner
          room = Room.new roomName 4 player
        in
          ( addRoom room lobby
          , pure ()
          )


--------------------------------------------------------------------------------
addRoom :: Room -> Lobby -> Lobby
addRoom room lobby@Lobby { lobbyRoomViews } =
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
addClient Client { clientChannel, clientId } lobby@Lobby { lobbyClientQueues } =
  lobby
    { lobbyClientQueues =
        Map.insert clientId clientChannel lobbyClientQueues
    }
