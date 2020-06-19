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
import           Client  (Client (..))
import qualified Client
import           Message (ClientMessage (..))
import qualified Message
import qualified Room
import           Server  (Server (..))
import qualified Server
import           Tickets (Ticket)


--------------------------------------------------------------------------------
data Error
  = RoomInvalidCapacity
  | RoomNoEntry
  | RoomInGame
  | RoomFull
  | RoomHasPlayer


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
  void . traverse (send message) . clients


--------------------------------------------------------------------------------
handleMessage :: Client -> Server -> ClientMessage -> Either Error Server
handleMessage client server message =
  case message of
    NewRoom name capacity ->
      if capacity > 0 && capacity <= Room.maxCapacity then
        Right $ Server.addRoom name capacity server
      else
        Left InvalidRoomCapacity

    JoinRoom playerName roomName ->
      (case Server.getRoom roomName server of
        Nothing ->
          Left RoomNoEntry

        Just room ->
          if | Room.inGame room ->
                Left RoomInGame

             | Room.isFull room ->
                Left RoomFull

             | Room.hasPlayertag playerName room ->
                Left RoomHasPlayer

             | otherwise ->
                let
                  newRoom = Room.addPlayer playerName room
                in
                  Server.addRoom newRoom server)

    LeaveRoom playerName roomName ->
      (case Server.getRoom roomName server of
        Nothing ->
          Left RoomNoEntry

        Just room -> Right $
          case Room.removePlayer playerName room of
            Nothing ->
              Server.removeRoom roomName server

            Just newRoom ->
              Server.addRoom roomName newRoom server)





      flip Server.updateRoom server . fst
      <$> ( Room.addPlayer playerName client
      =<< Server.getRoom roomName server )

    LeaveRoom playerName roomName ->
      flip (Server.maybeUpdateRoom roomName) server
      . Room.removePlayer playerName
      <$> Server.getRoom roomName server


--------------------------------------------------------------------------------
app :: MVar Server -> ServerApp
app mServer pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    ticket <- WS.receiveData connection
    server <- readMVar mServer

    case () of
      _ | not $ Server.isPendingTicket ticket server ->
          close ("Invalid authentication ticket" :: Text) ticket connection

        | Server.clientExists ticket server ->
          close ("Client already connected" :: Text) ticket connection

        | otherwise ->
            finally (onConnect >> onMessage) onDisconnect
        where
          onConnect = do
            newServer <- modifyMVar mServer $ \s ->
              let
                s' = Server.removePendingTicket ticket
                   $ Server.addClient ticket connection s
              in
                return ( s', s' )

            T.putStrLn $ "Client with ticket " <> ticket <> " connected"

            send (Message.listRooms newServer) connection

          onMessage = forever $ do
            message <- JSON.eitherDecode <$> WS.receiveData connection

            modifyMVar_ mServer $ \s ->
              case left T.pack message >>= handleMessage client s of
                Left errMsg ->
                  -- T.putStrLn errMsg (add logging levels)
                  send errMsg client
                  >> return s

                Right s' ->
                  return s'

          onDisconnect = do
            modifyMVar_ mServer $ return . Server.removeClient ticket
            T.putStrLn $ "Client with ticket " <> ticket <> " disconnected"
