module Scrabble.Client
  ( Client (..)
  , joinRoom
  , leaveRoom
  , new
  , send
  ) where


--------------------------------------------------------------------------------
import           Data.Text          (Text)
import           Network.WebSockets (Connection)

import           Scrabble.Types     (Client (..), EventQueue, Room)

import qualified Network.WebSockets as WS


--------------------------------------------------------------------------------
send :: Client -> Text -> IO ()    -- todo: custom message type
send Client { clientConnection } =
  WS.sendTextData clientConnection


--------------------------------------------------------------------------------
joinRoom :: EventQueue Room -> Client -> Client
joinRoom roomQueue client =
  client { clientRoomQueue = Just roomQueue }


--------------------------------------------------------------------------------
leaveRoom :: Client -> Client
leaveRoom client =
  client { clientRoomQueue = Nothing }


--------------------------------------------------------------------------------
new :: Connection -> Text -> EventQueue Client -> Client
new connection clientId queue = Client
  { clientConnection = connection
  , clientId         = clientId
  , clientQueue      = queue
  , clientRoomQueue  = Nothing
  }
