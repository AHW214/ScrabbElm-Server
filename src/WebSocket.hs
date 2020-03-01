{-# LANGUAGE OverloadedStrings #-}

module WebSocket
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow      (left)
import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import           Control.Exception  (finally)
import           Control.Monad      (forever, forM_)
import           Data.Aeson         as JSON
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Network.WebSockets (WebSocketsData, ServerApp)
import qualified Network.WebSockets as WS


--------------------------------------------------------------------------------
import           Client             (Client (..))
import qualified Client
import           Message            (ClientMessage (..))
import qualified Message
import qualified Room
import           Server             (Server (..))
import qualified Server


--------------------------------------------------------------------------------
close :: WebSocketsData a => a -> Client -> IO ()
close message Client { connection = c, ticket = t } =
  do T.putStrLn $ "Disconnecting client with ticket " <> t
     WS.sendClose c message


--------------------------------------------------------------------------------
send :: WebSocketsData a => a -> Client -> IO ()
send message Client { connection = c } =
  WS.sendTextData c message


--------------------------------------------------------------------------------
broadcast :: WebSocketsData a => a -> Server -> IO ()
broadcast message Server { clients = cs } =
  forM_ cs (send message)


--------------------------------------------------------------------------------
handleMessage :: Client -> Server -> ClientMessage -> Either Text Server
handleMessage client server message =
  case message of
    NewRoom name capacity ->
      Server.createRoom name capacity server

    JoinRoom playerName roomName ->
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
    let client = Client.new ticket connection

    server <- readMVar mServer

    case () of
      _ | not $ Server.isPendingTicket ticket server ->
          close ("Invalid authentication ticket" :: Text) client

        | Server.clientExists client server ->
          close ("Client already connected" :: Text) client

        | otherwise ->
            finally (onConnect >> onMessage) onDisconnect
        where
          onConnect = do
            newServer <- modifyMVar mServer $ \s ->
              let s' = Server.removePendingTicket ticket $ Server.addClient client s in return ( s', s' )
            T.putStrLn $ "Client with ticket " <> Client.ticket client <> " connected"

            send (Message.listRooms newServer) client

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
            modifyMVar_ mServer $ return . Server.removeClient client
            T.putStrLn $ "Client with ticket " <> Client.ticket client <> " disconnected"
