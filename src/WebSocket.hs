module WebSocket
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow      (left)
import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forever, void)
import           Data.Aeson         as JSON
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Network.WebSockets (Connection, WebSocketsData, ServerApp)
import qualified Network.WebSockets as WS


--------------------------------------------------------------------------------
import           Message (ClientMessage (..))
import qualified Message
import qualified Player
import qualified Room
import           Server  (Server)
import qualified Server
import           Tickets (Ticket)


--------------------------------------------------------------------------------
data Error
  = ClientExists
  | RoomExists
  | RoomFull
  | RoomHasPlayer
  | RoomInGame
  | RoomInvalidCapacity
  | RoomMissingPlayer
  | RoomNoEntry
  | TicketInvalid
  deriving Show


--------------------------------------------------------------------------------
close :: WebSocketsData a => a -> Ticket -> Connection -> IO ()
close message ticket conn = do
  T.putStrLn $ "Disconnecting client with ticket " <> ticket
  WS.sendClose conn message


--------------------------------------------------------------------------------
send :: WebSocketsData a => a -> Connection -> IO ()
send = flip WS.sendTextData


--------------------------------------------------------------------------------
broadcast :: WebSocketsData a => a -> Server -> IO ()
broadcast message =
  void . traverse (send message) . Server.connections


--------------------------------------------------------------------------------
handleMessage :: Connection -> Server -> ClientMessage -> Either Error Server
handleMessage conn server message =
  case message of
    NewRoom name capacity ->
      case Server.getRoom name server of
        Nothing ->
          if capacity > 0 && capacity <= Room.maxCapacity then
            Right $ Server.addRoom (Room.new name capacity) server
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
                  player = Player.new playerName conn
                  newRoom = Room.addPlayer player room
                in
                  Right $ Server.addRoom newRoom server

        _ ->
          Left RoomNoEntry

    LeaveRoom playerName roomName ->
      case Server.getRoom roomName server of
        Just room ->
          case Room.getPlayer playerName room of
            Just player ->
              let
                newRoom =
                  Room.removePlayer player room

                update =
                  if Room.isEmpty newRoom then
                    Server.removeRoom
                  else
                    Server.addRoom
              in
                Right $ update newRoom server

            _ -> Left RoomMissingPlayer

        _ -> Left RoomNoEntry


--------------------------------------------------------------------------------
app :: MVar Server -> ServerApp
app mServer pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    ticket <- WS.receiveData connection
    server <- readMVar mServer

    let disconnect err =
          close (T.pack $ show err) ticket connection

    case () of
      _ | not $ Server.isPendingTicket ticket server ->
            disconnect TicketInvalid

      _ | Server.connectionExists ticket server ->
            disconnect ClientExists

      _ | otherwise ->
            finally (onConnect >> onMessage) onDisconnect
        where
          onConnect = do
            newServer <- modifyMVar mServer $ \s ->
              let
                s' = Server.removePendingTicket ticket
                  $ Server.acceptConnection ticket connection s
              in
                return ( s', s' )

            T.putStrLn $ "Client with ticket " <> ticket <> " connected"

            send (Message.listRooms newServer) connection

          onMessage = forever $ do
            message <- JSON.eitherDecode <$> WS.receiveData connection

            modifyMVar_ mServer $ \s ->
              case left T.pack message >>= left (T.pack . show) . handleMessage connection s of
                Left errMsg ->
                  -- T.putStrLn errMsg (add logging levels)
                  send errMsg connection
                  >> return server

                Right newServer ->
                  return newServer

          onDisconnect = do
            modifyMVar_ mServer $ return . Server.removeConnection ticket
            T.putStrLn $ "Client with ticket " <> ticket <> " disconnected"
