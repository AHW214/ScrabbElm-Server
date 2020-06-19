module Player
  ( Player (..)
  , hasName
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as JSON
import           Data.Text  (Text)


--------------------------------------------------------------------------------
import           Client     (Client)


--------------------------------------------------------------------------------
data Player
  = Player
      { client   :: Client
      , name     :: Text
      , playerId :: Int
      , score    :: Int
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
new :: Text -> Client -> Player
new name client =
  Player
    { client   = client
    , name     = name
    , playerId = 0 -- TODO
    , score    = 0
    }


--------------------------------------------------------------------------------
hasName :: Text -> Player -> Bool
hasName n = (n ==) . name
