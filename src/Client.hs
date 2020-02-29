module Client
  ( Client(..)
  , new
  , hasTicket
  ) where

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.WebSockets (Connection)
import Tickets (Ticket)

data Client
  = Client
      { ticket :: Text
      , connection :: Connection
      }

new :: Ticket -> Connection -> Client
new ticket connection =
  Client
    { ticket = ticketText
    , connection = connection
    }
  where
    ticketText = toStrict $ decodeUtf8 ticket

hasTicket :: Text -> Client -> Bool
hasTicket ticket Client { ticket = t } =
  ticket == t
