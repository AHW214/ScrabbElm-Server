module Client
  ( Client (..)
  , hasTicket
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Text               (Text)
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.WebSockets      (Connection)


--------------------------------------------------------------------------------
import           Tickets                 (Ticket)


--------------------------------------------------------------------------------
data Client
  = Client
      { connection :: Connection
      , ticket     :: Text
      }


--------------------------------------------------------------------------------
new :: Connection -> Ticket -> Client
new connection =
  Client connection . toStrict . decodeUtf8


--------------------------------------------------------------------------------
hasTicket :: Text -> Client -> Bool
hasTicket t = (t ==) . ticket
