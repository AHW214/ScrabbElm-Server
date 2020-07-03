module Scrabble.Message
  ( Message (..)
  , ClientMessage (..)
  , ServerMessage (..)
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow           (left)
import           Data.Foldable           (traverse_)
import           Data.Text               (Text)

import           Scrabble.Client         (Client (..))
import           Scrabble.Message.Client (ClientMessage (..))
import           Scrabble.Message.Server (ServerMessage (..))
import           Scrabble.Server         (Server (..))

import qualified Data.Aeson              as JSON
import qualified Data.Text               as Text
import qualified Network.WebSockets      as WS

import qualified Scrabble.Server         as Server


--------------------------------------------------------------------------------
class Monad m => Message m where
  broadcastClients :: Server -> ServerMessage -> m ()

  broadcastLobby :: Server -> ServerMessage -> m ()

  fromClient :: Client -> m (Either Text ClientMessage)

  toClient :: Client -> ServerMessage -> m ()

  toClients :: Foldable t => t Client -> ServerMessage -> m ()


--------------------------------------------------------------------------------
instance Message IO where
  broadcastClients Server { serverConnectedClients } =
    toClients serverConnectedClients

  broadcastLobby server =
    toClients (Server.clientsInLobby server)

  fromClient =
    fmap (left Text.pack . JSON.eitherDecodeStrict')
    . WS.receiveData
    . clientConnection

  toClient Client { clientConnection } =
    WS.sendTextData clientConnection . JSON.encode

  toClients clients message =
    traverse_ (flip toClient message) clients
