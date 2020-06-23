module Scrabble.WebSocket
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow           (left)
import           Control.Concurrent      (MVar, modifyMVar, modifyMVar_, readMVar)
import           Control.Exception       (finally)
import           Control.Monad           (forever, void)
import           Data.Functor            ((<&>))
import           Data.Text               (Text)
import           Network.WebSockets      (Connection, WebSocketsData, ServerApp)

import           Scrabble.Message        (ClientMessage (..))
import           Scrabble.Room           (Room (..))
import           Scrabble.Server         (Server (..))

import qualified Data.ByteString         as BSS
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Network.WebSockets      as WS

import qualified Scrabble.Message        as Message
import qualified Scrabble.Player         as Player
import qualified Scrabble.Room           as Room
import qualified Scrabble.Server         as Server


--------------------------------------------------------------------------------
type Client =
  ( Text, Connection )


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
close :: Text -> Connection -> IO ()
close reason connection = do
  T.putStrLn $ T.unlines
    [ "Disconnecting client"
    , "Reason: " <> reason
    ]

  WS.sendClose connection reason


--------------------------------------------------------------------------------
send :: WebSocketsData a => a -> Connection -> IO ()
send = flip WS.sendTextData


--------------------------------------------------------------------------------
sendAll :: (WebSocketsData a, Traversable t) => a -> t Connection -> IO ()
sendAll message =
  void . traverse (send message)


--------------------------------------------------------------------------------
sendWhen :: WebSocketsData a => (Text -> Bool) -> a -> Server -> IO ()
sendWhen predicate message =
  sendAll message . Server.clientsWho predicate


--------------------------------------------------------------------------------
broadcast :: WebSocketsData a => a -> Server -> IO ()
broadcast message =
  sendAll message . serverConnectedClients


--------------------------------------------------------------------------------
decodeMessage :: BSS.ByteString -> Either Error ClientMessage
decodeMessage =
  left MessageInvalid . Message.eitherDecode


--------------------------------------------------------------------------------
handleMessage :: Client -> Server -> ClientMessage -> Either Error ( Server, IO () )
handleMessage client@( clientId, clientConn ) server message =
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
                    ( Server.joinRoom clientId roomName
                      $ Server.addRoom newRoom server
                    , send (Message.joinRoom newRoom) clientConn
                    )

        _ ->
          Left RoomNoEntry

    LeaveRoom ->
      case Server.getClientRoom clientId server of
        Just room ->
          case Room.getPlayer clientId room of
            Just player ->
              let
                newRoom =
                  Room.removePlayer player room

                name =
                  roomName newRoom

                ( update, action ) =
                  if Room.isEmpty newRoom then
                    ( \r ->
                        Server.leaveRoom clientId
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
                  , send (Message.leaveRoom name) clientConn
                    >> action
                  )

            _ -> Left RoomMissingPlayer

        _ -> Left RoomNoEntry

    _ ->
      Right ( server, pure () )


--------------------------------------------------------------------------------
authenticate :: MVar Server -> Client -> Text -> IO ()
authenticate mServer client@( clientId, clientConn ) clientTicket =
  readMVar mServer <&> Server.getPendingClient clientId >>=
    \case
      Nothing ->
        disconnect AuthIdentityInvalid

      Just ticket | ticket /= clientTicket ->
        disconnect AuthTicketInvalid

      _ ->
        finally (onConnect >> onMessage) onDisconnect
  where
    onConnect = do
      newServer <- modifyMVar mServer $ \s ->
        let
          s' = Server.acceptPendingClient clientId clientConn s
        in
          pure ( s', s' )

      T.putStrLn $ "Client " <> clientId <> " connected"

      send (Message.listRooms newServer) clientConn

    onMessage = forever $ do
      message <- decodeMessage <$> WS.receiveData clientConn

      modifyMVar_ mServer $ \s ->
        case message >>= handleMessage client s of
          Left err ->
            -- T.putStrLn errMsg (add logging levels)
            send (T.pack $ show err) clientConn
            >> pure s

          Right ( s', action ) ->
            action >> pure s'

    onDisconnect = do
      modifyMVar_ mServer $ pure . Server.removeConnectedClient clientId
      T.putStrLn $ "Client " <> clientId <> " disconnected"

    disconnect err =
      close (T.pack $ show err) clientConn


--------------------------------------------------------------------------------
app :: MVar Server -> ServerApp
app mServer pending = do
  clientConn <- WS.acceptRequest pending
  WS.withPingThread clientConn 30 (pure ()) $
    WS.receiveData clientConn <&> decodeMessage >>=
      \case
        Right (Authenticate clientId clientTicket) ->
          authenticate mServer ( clientId, clientConn ) clientTicket

        _ ->
          close (T.pack $ show AuthFormatInvalid) clientConn
