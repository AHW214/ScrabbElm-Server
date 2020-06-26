module Scrabble.WebSocket
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow      (left)
import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forever, void)
import           Data.Text          (Text)
import           Network.WebSockets (Connection, WebSocketsData, ServerApp)

import           Scrabble.Client    (Client (..))
import           Scrabble.Message   (ClientMessage (..))
import           Scrabble.Room      (Room (..))
import           Scrabble.Server    (Server (..))

import qualified Data.ByteString    as BSS
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text
import qualified Network.WebSockets as WS

import qualified Scrabble.Client    as Client
import qualified Scrabble.Message   as Message
import qualified Scrabble.Player    as Player
import qualified Scrabble.Room      as Room
import qualified Scrabble.Server    as Server


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
app :: MVar Server -> ServerApp
app mServer pending = do
  clientConn <- WS.acceptRequest pending
  WS.withPingThread clientConn 30 (pure ()) $
    WS.receiveData clientConn >>= \received ->
      case decodeMessage received of
        Right (Authenticate clientId clientTicket) ->
          authenticate mServer (Client.new clientConn clientId) clientTicket

        _ ->
          close (Text.pack $ show AuthFormatInvalid) clientConn


--------------------------------------------------------------------------------
authenticate :: MVar Server -> Client -> Text -> IO ()
authenticate mServer client@Client { clientConnection, clientId } clientTicket =
  readMVar mServer >>= \server ->
    case Server.getPendingClient clientId server of
      Nothing ->
        disconnect AuthIdentityInvalid

      Just ticket | ticket /= clientTicket ->
        disconnect AuthTicketInvalid

      _ ->
        finally (onConnect >> onMessage) onDisconnect
  where
    onConnect :: IO ()
    onConnect = do
      newServer <- modifyMVar mServer $ \s ->
        let
          s' = Server.acceptPendingClient clientId clientConnection s
        in
          pure ( s', s' )

      Text.putStrLn $ "Client " <> clientId <> " connected"

      send (Message.listRooms newServer) clientConnection

    onMessage :: IO ()
    onMessage = forever $ do
      message <- decodeMessage <$> WS.receiveData clientConnection

      modifyMVar_ mServer $ \s ->
        case message >>= handleMessage client s of
          Left err ->
            -- Text.putStrLn errMsg (add logging levels)
            send (Text.pack $ show err) clientConnection
            >> pure s

          Right ( s', action ) ->
            action >> pure s'

    onDisconnect :: IO ()
    onDisconnect = do
      modifyMVar_ mServer $ pure . Server.removeConnectedClient clientId
      Text.putStrLn $ "Client " <> clientId <> " disconnected"

    disconnect :: Error -> IO ()
    disconnect err =
      close (Text.pack $ show err) clientConnection


--------------------------------------------------------------------------------
decodeMessage :: BSS.ByteString -> Either Error ClientMessage
decodeMessage =
  left MessageInvalid . Message.eitherDecode


--------------------------------------------------------------------------------
handleMessage :: Client -> Server -> ClientMessage -> Either Error ( Server, IO () )
handleMessage client@Client { clientConnection } server message =
  case message of
    NewRoom name capacity ->
      case Server.getRoom name server of
        Nothing ->
          if capacity > 0 && capacity <= Room.maxCapacity then
            let
              newRoom = Room.new name capacity
            in
              Right
                ( Server.addRoom newRoom server
                , sendWhen
                    (not . flip Server.inRoom server)
                    (Message.newRoom newRoom)
                    server
                )
          else
            Left RoomInvalidCapacity

        _ ->
          Left RoomExists

    JoinRoom playerName roomName ->
      case Server.getRoom roomName server of
        Just room ->
          if | Room.inGame room ->
                Left RoomInGame

             | Room.isFull room ->
                Left RoomFull

             | Room.hasPlayerTag playerName room ->
                Left RoomHasPlayer

             | otherwise ->
                let
                  newPlayer = Player.new playerName client
                  newRoom = Room.addPlayer newPlayer room
                in
                  Right
                    ( Server.joinRoom client roomName
                      $ Server.addRoom newRoom server
                    , send (Message.joinRoom newRoom) clientConnection
                    )

        _ ->
          Left RoomNoEntry

    LeaveRoom ->
      case Server.getClientRoom client server of
        Just room ->
          case Room.getPlayer client room of
            Just player ->
              let
                newRoom =
                  Room.removePlayer player room

                name =
                  roomName newRoom

                ( update, action ) =
                  if Room.isEmpty newRoom then
                    ( \r ->
                        Server.leaveRoom client
                        . Server.removeRoom r
                    , sendWhen
                        (not . flip Server.inRoom server)
                        (Message.removeRoom name)
                        server
                    )
                  else
                    ( Server.addRoom
                    , pure ()
                    )
              in
                Right
                  ( update newRoom server
                  , send (Message.leaveRoom name) clientConnection
                    >> action
                  )

            _ -> Left RoomMissingPlayer

        _ -> Left RoomNoEntry

    _ ->
      Right ( server, pure () )


--------------------------------------------------------------------------------
broadcast :: WebSocketsData a => a -> Server -> IO ()
broadcast message =
  sendAll message . serverConnectedClients


--------------------------------------------------------------------------------
sendWhen :: WebSocketsData a => (Text -> Bool) -> a -> Server -> IO ()
sendWhen predicate message =
  sendAll message . Server.clientsWho predicate


--------------------------------------------------------------------------------
sendAll :: (WebSocketsData a, Traversable t) => a -> t Connection -> IO ()
sendAll message =
  void . traverse (send message)


--------------------------------------------------------------------------------
send :: WebSocketsData a => a -> Connection -> IO ()
send = flip WS.sendTextData


--------------------------------------------------------------------------------
close :: Text -> Connection -> IO ()
close reason connection = do
  Text.putStrLn $ Text.unlines
    [ "Disconnecting client"
    , "Reason: " <> reason
    ]

  WS.sendClose connection reason
