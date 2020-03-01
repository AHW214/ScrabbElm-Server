{-# LANGUAGE OverloadedStrings #-}

module Player
  ( Player (..)
  , new
  , hasName
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as JSON
import           Data.Text  (Text)
import           Prelude    hiding (id)


--------------------------------------------------------------------------------
import           Client     (Client)


--------------------------------------------------------------------------------
data Player
  = Player
      { id :: Int
      , name :: Text
      , score :: Int
      , client :: Client
      }


--------------------------------------------------------------------------------
instance ToJSON Player where
  toJSON Player { name = n, score = s } =
    JSON.object
      [ "name" .= n
      , "score" .= s
      ]

  toEncoding Player { name = n, score = s } =
    JSON.pairs
      $ "name" .= n
      <> "score" .= s


--------------------------------------------------------------------------------
new :: Text -> Client -> Player
new name client =
  Player
    { id = 0
    , name = name
    , score = 0
    , client = client
    }


--------------------------------------------------------------------------------
hasName :: Text -> Player -> Bool
hasName name Player { name = n } =
  name == n
