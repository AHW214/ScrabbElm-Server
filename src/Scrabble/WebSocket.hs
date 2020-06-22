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

import           Scrabble.Message   (ClientMessage (..))
import           Scrabble.Room      (Room (..))
import           Scrabble.Server    (Server (..))
import           Scrabble.Tickets   (Ticket)

import           Data.ByteString    as BSS
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS

import qualified Scrabble.Message   as Message
import qualified Scrabble.Player    as Player
import qualified Scrabble.Room      as Room
import qualified Scrabble.Server    as Server


--------------------------------------------------------------------------------
type Client =
  ( Ticket, Connection )


--------------------------------------------------------------------------------
data Error
  = ClientExists
  | MessageInvalid Text
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
close :: Text -> Client -> IO ()
close reason ( ticket, conn ) = do
  T.putStrLn $ T.unlines
    [ "Disconnecting client with ticket " <> ticket
    , "Reason: " <> reason
    ]

  WS.sendClose conn reason


--------------------------------------------------------------------------------
send :: WebSocketsData a => a -> Connection -> IO ()
send = flip WS.sendTextData


--------------------------------------------------------------------------------
sendAll :: (WebSocketsData a, Traversable t) => a -> t Connection -> IO ()
sendAll message =
  void . traverse (send message)


--------------------------------------------------------------------------------
sendWhen :: WebSocketsData a => (Ticket -> Bool) -> a -> Server -> IO ()
sendWhen predicate message =
  sendAll message . Server.clientsWho predicate


--------------------------------------------------------------------------------
broadcast :: WebSocketsData a => a -> Server -> IO ()
broadcast message =
  sendAll message . serverConnections


--------------------------------------------------------------------------------
decodeMessage :: BSS.ByteString -> Either Error ClientMessage
decodeMessage =
  left MessageInvalid . Message.eitherDecode


--------------------------------------------------------------------------------
handleMessage :: Client -> Server -> ClientMessage -> Either Error ( Server, IO () )
handleMessage client@( ticket, conn ) server message =
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
                    ( Server.joinRoom ticket roomName
                      $ Server.addRoom newRoom server
                    , send (Message.joinRoom newRoom) conn
                    )

        _ ->
          Left RoomNoEntry

    LeaveRoom ->
      case Server.getClientRoom ticket server of
        Just room ->
          case Room.getPlayer ticket room of
            Just player ->
              let
                newRoom =
                  Room.removePlayer player room

                name =
                  roomName newRoom

                ( update, action ) =
                  if Room.isEmpty newRoom then
                    ( \r ->
                        Server.leaveRoom ticket
                        . Server.removeRoom r
                    , sendWhen
                        (not . flip Server.inRoom server)
                        (Message.removeRoom name)
                        server
                    )
                  else
                    ( Server.addRoom
                    , return ()
                    )
              in
                Right
                  ( update newRoom server
                  , send (Message.leaveRoom name) conn
                    >> action
                  )

            _ -> Left RoomMissingPlayer

        _ -> Left RoomNoEntry


--------------------------------------------------------------------------------
app :: MVar Server -> ServerApp
app mServer pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    ticket <- WS.receiveData connection
    server <- readMVar mServer

    let client =
          ( ticket, connection )

    let disconnect err =
          close (T.pack $ show err) client

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
            message <- decodeMessage <$> WS.receiveData connection

            modifyMVar_ mServer $ \s ->
              case message >>= handleMessage client s of
                Left err ->
                  -- T.putStrLn errMsg (add logging levels)
                  send (T.pack $ show err) connection
                  >> return s

                Right ( s', action ) ->
                  action >> return s'

          onDisconnect = do
            modifyMVar_ mServer $ return . Server.removeConnection ticket
            T.putStrLn $ "Client with ticket " <> ticket <> " disconnected"
