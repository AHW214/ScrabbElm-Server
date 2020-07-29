module Scrabble.Client
  ( Client (..)
  , new
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (STM, TChan)
import           Data.Text              (Text)
import           Network.WebSockets     (Connection)

-- import           Scrabble.Common     (ID)
import           Scrabble.Message       (Inbound, Message)

import qualified Control.Concurrent.STM as STM


--------------------------------------------------------------------------------
data Client = Client
  { clientConnection    :: Connection
  , clientId            :: Text
  , clientMessageChan   :: TChan (Message Inbound)
  }


--------------------------------------------------------------------------------
new :: Connection -> Text -> STM Client
new connection =
  flip fmap STM.newTChan . Client connection
