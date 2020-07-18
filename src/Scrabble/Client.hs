module Scrabble.Client
  ( Client (..)
  , new
  , send
  ) where


--------------------------------------------------------------------------------
import           Data.Text          (Text)
import           Network.WebSockets (Connection)

import           Scrabble.Types     (Client (..), EventQueue)

import qualified Network.WebSockets as WS


--------------------------------------------------------------------------------
send :: Client -> Text -> IO ()    -- todo: custom message type
send Client { clientConnection } =
  WS.sendTextData clientConnection


--------------------------------------------------------------------------------
new :: Connection -> Text -> EventQueue Client -> Client
new = Client
