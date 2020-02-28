{-# LANGUAGE OverloadedStrings #-}

module WebSocket
  ( app
  ) where

import Client (Client(..))
import qualified Client
import Control.Concurrent (MVar, modifyMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Server (Server(..))
import qualified Server

close :: Text -> Client -> IO ()
close message (Client { connection = c, ticket = t }) =
  do T.putStrLn $ "Disconnecting client with ticket " <> t
     WS.sendClose c message

send :: Text -> Client -> IO ()
send message (Client { connection = c }) =
  WS.sendTextData c message

broadcast :: Text -> Server -> IO ()
broadcast message (Server { clients = cs }) =
  forM_ cs (send message)

app :: MVar Server -> WS.ServerApp
app mServer pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    ticket <- WS.receiveData connection
    let client = Client.new ticket connection

    server <- readMVar mServer

    case () of
      _ | not $ Server.isPendingTicket ticket server ->
          close "Invalid authentication ticket" client

        | Server.clientExists client server ->
          close "Client already connected" client

        | otherwise ->
            finally connect disconnect
        where
          connect = do
            newServer <- modifyMVar mServer $ \m ->
              let m' = Server.removePendingTicket ticket $ Server.addClient client m in return ( m', m' )
            T.putStrLn $ "Client with ticket " <> Client.ticket client <> " connected"

          disconnect = do
            newServer <- modifyMVar mServer $ \m ->
              let m' = Server.removeClient client m in return ( m', m' )
            T.putStrLn $ "Client with ticket " <> Client.ticket client <> " disconnected"
