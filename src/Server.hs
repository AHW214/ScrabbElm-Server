{-# LANGUAGE OverloadedStrings #-}

module Server
  ( Model
  ) where

import Client
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception (finally)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.List as List
import Network.WebSockets as WS
import Room (Room)
import Tickets (Ticket)

data Model
  = Model
      { pendingTickets :: [ Ticket ]
      , clients :: [ Client ]
      , rooms :: [ Room ]
      }

removePendingTicket :: Ticket -> Model -> Model
removePendingTicket ticket model =
  model { pendingTickets = List.delete ticket $ pendingTickets model }

isPendingTicket :: Ticket -> Model -> Bool
isPendingTicket ticket (Model { pendingTickets = ts }) =
  elem ticket ts

addClient :: Client -> Model -> Model
addClient client model =
  model { clients = client : clients model }

removeClient :: Client -> Model -> Model
removeClient (Client { ticket = t }) model =
  model { clients = filter (Client.hasTicket t) $ clients model }

clientExists :: Client -> Model -> Bool
clientExists (Client { ticket = t }) (Model { clients = cs }) =
  List.any (Client.hasTicket t) cs

close :: Text -> Client -> IO ()
close message (Client { connection = c, ticket = t }) =
  do T.putStrLn $ "Disconnecting client with ticket " <> t
     WS.sendClose c message

send :: Text -> Client -> IO ()
send message (Client { connection = c }) =
  WS.sendTextData c message

app :: MVar Model -> WS.ServerApp
app mModel pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (return ()) $ do
    ticket <- WS.receiveData connection
    let client = Client.new ticket connection

    model <- readMVar mModel

    case () of
      _ | not $ isPendingTicket ticket model ->
          close "Invalid authentication ticket" client

        | clientExists client model ->
          close "Client already connected" client

        | otherwise ->
            finally connect disconnect
        where
          connect = do
            newModel <- modifyMVar mModel $ \m ->
              let m' = removePendingTicket ticket $ addClient client m in return ( m', m' )
            T.putStrLn $ "Client with ticket " <> Client.ticket client <> " connected"

          disconnect = do
            newModel <- modifyMVar mModel $ \m ->
              let m' = removeClient client m in return ( m', m' )
            T.putStrLn $ "Client with ticket " <> Client.ticket client <> " disconnected"
