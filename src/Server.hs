{-# LANGUAGE OverloadedStrings #-}

module Server
  ( Server(..)
  , addClient
  , addPendingTicket
  , clientExists
  , isPendingTicket
  , new
  , removeClient
  , removePendingTicket
  ) where

import Client (Client(..))
import qualified Client
import qualified Data.List as List
import Room (Room)
import Tickets (Ticket)

data Server
  = Server
      { pendingTickets :: [ Ticket ]
      , clients :: [ Client ]
      , rooms :: [ Room ]
      }

new :: Server
new =
  Server
    { pendingTickets = []
    , clients = []
    , rooms = []
    }

addPendingTicket :: Ticket -> Server -> Server
addPendingTicket ticket server =
  server { pendingTickets = ticket : pendingTickets server }

removePendingTicket :: Ticket -> Server -> Server
removePendingTicket ticket server =
  server { pendingTickets = List.delete ticket $ pendingTickets server }

isPendingTicket :: Ticket -> Server -> Bool
isPendingTicket ticket (Server { pendingTickets = ts }) =
  elem ticket ts

addClient :: Client -> Server -> Server
addClient client server =
  server { clients = client : clients server }

removeClient :: Client -> Server -> Server
removeClient (Client { ticket = t }) server =
  server { clients = filter (not . Client.hasTicket t) $ clients server }

clientExists :: Client -> Server -> Bool
clientExists (Client { ticket = t }) (Server { clients = cs }) =
  List.any (Client.hasTicket t) cs
