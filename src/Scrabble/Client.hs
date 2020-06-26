module Scrabble.Client
  ( Client (..)
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Text          (Text)
import           Network.WebSockets (Connection)


--------------------------------------------------------------------------------
data Client = Client
  { clientConnection :: Connection
  , clientId :: Text
  }


--------------------------------------------------------------------------------
instance Eq Client where
 Client { clientId = id1 } == Client { clientId = id2 } =
   id1 == id2


--------------------------------------------------------------------------------
instance Ord Client where
  compare Client { clientId = id1 } Client { clientId = id2 } =
    compare id1 id2


--------------------------------------------------------------------------------
new :: Connection -> Text -> Client
new = Client
