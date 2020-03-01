{-# LANGUAGE OverloadedStrings #-}

module Server
  ( Server (..)
  , addClient
  , addPendingTicket
  , clientExists
  , createRoom
  , getRoom
  , isPendingTicket
  , maybeUpdateRoom
  , new
  , removeClient
  , removePendingTicket
  , removeRoom
  , updateRoom
  ) where


--------------------------------------------------------------------------------
import qualified Data.List     as List
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Text     (Text)


--------------------------------------------------------------------------------
import           Client        (Client (..))
import qualified Client
import           Room          (Room (..))
import qualified Room
import           Tickets       (Ticket)


--------------------------------------------------------------------------------
data Server
  = Server
      { pendingTickets :: [ Ticket ]
      , clients :: [ Client ]
      , rooms :: Map Text Room
      }


--------------------------------------------------------------------------------
new :: Server
new =
  Server
    { pendingTickets = []
    , clients = []
    , rooms = Map.empty
    }


--------------------------------------------------------------------------------
addPendingTicket :: Ticket -> Server -> Server
addPendingTicket ticket server =
  server { pendingTickets = ticket : pendingTickets server }


--------------------------------------------------------------------------------
removePendingTicket :: Ticket -> Server -> Server
removePendingTicket ticket server =
  server { pendingTickets = List.delete ticket $ pendingTickets server }


--------------------------------------------------------------------------------
isPendingTicket :: Ticket -> Server -> Bool
isPendingTicket ticket Server { pendingTickets = ts } =
  elem ticket ts


--------------------------------------------------------------------------------
addClient :: Client -> Server -> Server
addClient client server =
  server { clients = client : clients server }


--------------------------------------------------------------------------------
removeClient :: Client -> Server -> Server
removeClient Client { ticket = t } server =
  server { clients = filter (not . Client.hasTicket t) $ clients server }


--------------------------------------------------------------------------------
clientExists :: Client -> Server -> Bool
clientExists Client { ticket = t } Server { clients = cs } =
  any (Client.hasTicket t) cs


--------------------------------------------------------------------------------
createRoom :: Text -> Int -> Server -> Either Text Server
createRoom roomName capacity server@Server { rooms = rs } =
  if Map.member roomName rs then
    Left $ "Room with name '" <> roomName <> "' already exists"
  else
    (\r -> server { rooms = Map.insert roomName r rs })
    <$> Room.new roomName capacity


--------------------------------------------------------------------------------
updateRoom :: Room -> Server -> Server
updateRoom room@Room { name = n } server@Server { rooms = rs } =
  server { rooms = Map.insert n room rs }


--------------------------------------------------------------------------------
maybeUpdateRoom :: Text -> Maybe Room -> Server -> Server
maybeUpdateRoom roomName maybeRoom server =
  case maybeRoom of
    Just room ->
      updateRoom room server

    _ ->
      removeRoom roomName server


--------------------------------------------------------------------------------
removeRoom :: Text -> Server -> Server
removeRoom roomName server@Server { rooms = rs } =
  server { rooms = Map.delete roomName rs }


--------------------------------------------------------------------------------
getRoom :: Text -> Server -> Either Text Room
getRoom name Server { rooms = rs } =
  case Map.lookup name rs of
    Just room ->
      Right room

    _ ->
      Left $ "Room with name '" <> name <> "' does not exist"
