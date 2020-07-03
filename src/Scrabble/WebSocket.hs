module Scrabble.WebSocket
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow           (left)
import           Control.Concurrent      (MVar, modifyMVar_, readMVar)
import           Control.Exception       (finally)
import           Control.Monad           (forever)
import           Data.Text               (Text)
import           Network.WebSockets      (Connection, ServerApp)
import           TextShow                (TextShow (..), FromStringShow (..))

import           Scrabble.Client         (Client (..))
import           Scrabble.Log            (Log (..))
import           Scrabble.Message        (Message (..), ClientMessage (..),
                                          ServerMessage (..))
import           Scrabble.Room           (Room (..))
import           Scrabble.Server         (Server (..))

import qualified Data.Text               as Text
import qualified Network.WebSockets      as WS

import qualified Scrabble.Authentication as Auth
import qualified Scrabble.Player         as Player
import qualified Scrabble.Room           as Room
import qualified Scrabble.Server         as Server


--------------------------------------------------------------------------------
data Error
  = AuthFormatInvalid
  | AuthTicketInvalid
  | AuthIdentityInvalid
  | MessageInvalid Text
  | RoomExists
  | RoomFull
  | RoomHasPlayer
  | RoomInGame
  | RoomInvalidCapacity
  | RoomMissingPlayer
  | RoomNoEntry
  deriving Show


--------------------------------------------------------------------------------
instance TextShow Error where
  showbPrec p = showbPrec p . FromStringShow


--------------------------------------------------------------------------------
app :: MVar Server -> ServerApp
app mServer pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (pure ()) $ do
    authResponse <- WS.receiveData connection
    server <- readMVar mServer

    case authenticate connection authResponse server of
      Right ( newServer, client@Client { clientId } ) -> do
        modifyMVar_ mServer $ pure . const newServer
        logInfo newServer $ "Client " <> clientId <> " connected"
        toClient client $ ServerListRooms (Server.previewRooms newServer)

        finally onMessage onDisconnect
        where
          onMessage :: IO ()
          onMessage = forever $ receiveMessage >>= \msg ->
            modifyMVar_ mServer $ \s -> case msg of
              Left err ->
                printError s err
                >> pure s

              Right message ->
                handleMessage client s message >>= \case
                  Left err ->
                    printError s err
                    >> toClient client (ServerError $ showt err)
                    >> pure s

                  Right s' ->
                    pure s'

          onDisconnect :: IO ()
          onDisconnect = modifyMVar_ mServer $ \s ->
            logInfo s ("Client " <> clientId <> " disconnected")
            >> pure (Server.removeConnectedClient client s)

          receiveMessage :: Message m => m (Either Error ClientMessage)
          receiveMessage =
            left MessageInvalid <$> fromClient client

      Left err -> do
        let reason = showt err

        logError server $ Text.unlines
          [ "Closing pending connection"
          , "Reason: " <> reason
          ]

        WS.sendClose connection reason


--------------------------------------------------------------------------------
authenticate :: Connection -> Text -> Server -> Either Error ( Server, Client )
authenticate clientConn clientJWT server@Server { serverAuthSecret } =
  case Auth.verifyClientJWT serverAuthSecret clientJWT of
    Just ( clientId, clientTicket ) ->
      case Server.getPendingClient clientId server of
        Nothing ->
          Left AuthIdentityInvalid

        Just ticket | ticket /= clientTicket ->
          Left AuthTicketInvalid

        _ ->
          Right $ Server.acceptPendingClient clientId clientConn server

    _ ->
      Left AuthFormatInvalid


--------------------------------------------------------------------------------
handleMessage
  :: forall m. (Log m, Message m)
  => Client
  -> Server
  -> ClientMessage
  -> m (Either Error Server)
handleMessage client server message =
  case message of
    ClientNewRoom name capacity ->
      case Server.getRoom name server of
        Nothing ->
          if capacity > 0 && capacity <= Room.maxCapacity then
            let
              newRoom = Room.new name capacity
              newRoomPreview = Room.toPreview newRoom
            in
              broadcastLobby server (ServerNewRoom newRoomPreview)
              >> res (Server.addRoom newRoom server)
          else
            err RoomInvalidCapacity

        _ ->
          err RoomExists

    ClientJoinRoom playerName roomName ->
      case Server.getRoom roomName server of
        Just room ->
          if | Room.inGame room ->
                err RoomInGame

             | Room.isFull room ->
                err RoomFull

             | Room.hasPlayerTag playerName room ->
                err RoomHasPlayer

             | otherwise ->
                let
                  newPlayer = Player.new playerName client
                  newRoom = Room.addPlayer newPlayer room
                in
                  toClient client (ServerJoinRoom newRoom)
                  >> res (Server.joinRoom client roomName
                         $ Server.addRoom newRoom server)

        _ ->
          err RoomNoEntry

    ClientLeaveRoom ->
      case Server.getClientRoom client server of
        Just room@Room { roomName } ->
          case Room.getPlayer client room of
            Just player ->
              let
                maybeRoom =
                  Room.removePlayer player room

                ( update, action ) =
                  case maybeRoom of
                    Nothing ->
                      ( Server.leaveRoom client . Server.removeRoom room
                      , broadcastLobby server $ ServerRemoveRoom roomName
                      )
                    Just newRoom ->
                      ( Server.addRoom newRoom
                      , pure ()
                      )
              in
                toClient client (ServerLeaveRoom roomName)
                >> action
                >> res (update server)

            _ -> err RoomMissingPlayer

        _ -> err RoomNoEntry
  where
    err :: Error -> m (Either Error Server)
    err = pure . Left

    res :: Server -> m (Either Error Server)
    res = pure . Right
