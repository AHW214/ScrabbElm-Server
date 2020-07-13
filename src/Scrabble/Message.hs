module Scrabble.Message
  ( Message (..)
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow      (left)
import           Data.Foldable      (traverse_)
import           Data.Text          (Text)

import           Scrabble.Types     (Client (..), Lobby (..), MessageInbound,
                                     MessageOutbound, Room)

import qualified Data.Aeson         as JSON
import qualified Data.Text          as Text
import qualified Network.WebSockets as WS

import qualified Scrabble.Room      as Room


--------------------------------------------------------------------------------
class Monad m => Message m where
  broadcastLobby :: Lobby -> MessageOutbound -> m ()

  broadcastRoom :: Room -> MessageOutbound -> m ()

  fromClient :: Client -> m (Either Text MessageInbound)

  toClient :: Client -> MessageOutbound -> m ()

  toClients :: Foldable t => t Client -> MessageOutbound -> m ()


--------------------------------------------------------------------------------
instance Message IO where
  broadcastLobby Lobby { lobbyClients } =
    toClients lobbyClients

  broadcastRoom room =
    toClients $ Room.getClients room

  fromClient =
    fmap (left Text.pack . JSON.eitherDecodeStrict')
    . WS.receiveData
    . clientConnection

  toClient Client { clientConnection } =
    WS.sendTextData clientConnection . JSON.encode

  toClients clients message =
    traverse_ (flip toClient message) clients
