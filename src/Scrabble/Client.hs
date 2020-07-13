module Scrabble.Client
  ( Client (..)
  , ClientEventExternal
  , ClientEventInternal
  , ClientEvent
  , ClientQueue
  , new
  , send
  ) where


--------------------------------------------------------------------------------
import           Data.Text          (Text)
import           Network.WebSockets (Connection)

import           Scrabble.Types     (Client (..), ClientEventExternal (..),
                                     ClientEventInternal (..), ClientEvent,
                                     ClientQueue)

import qualified Network.WebSockets as WS


--------------------------------------------------------------------------------
send :: Client -> Text -> IO ()    -- todo: custom message type
send Client { clientConnection } =
  WS.sendTextData clientConnection


--------------------------------------------------------------------------------
new :: Connection -> Text -> ClientQueue -> Client
new = Client
