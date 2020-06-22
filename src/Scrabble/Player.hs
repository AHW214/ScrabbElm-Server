module Player
  ( Player (..)
  , hasName
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson         (ToJSON, (.=))
import qualified Data.Aeson         as JSON
import           Data.Text          (Text)
import           Network.WebSockets (Connection)
import           Prelude            hiding (id)


--------------------------------------------------------------------------------
import           Tickets (Ticket)


--------------------------------------------------------------------------------
data Player
  = Player
      { client :: ( Ticket, Connection )
      , id     :: Int
      , name   :: Text
      , score  :: Int
      }


--------------------------------------------------------------------------------
instance ToJSON Player where
  toJSON Player { name, score } =
    JSON.object
      [ "name"  .= name
      , "score" .= score
      ]

  toEncoding Player { name, score } =
    JSON.pairs
      $ "name"   .= name
      <> "score" .= score


--------------------------------------------------------------------------------
new :: Text -> ( Ticket, Connection ) -> Player
new name client =
  Player
    { client = client
    , id     = 0 -- TODO
    , name   = name
    , score  = 0
    }


--------------------------------------------------------------------------------
hasName :: Text -> Player -> Bool
hasName n = (n ==) . name
