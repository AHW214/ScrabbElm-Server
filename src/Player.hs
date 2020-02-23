{-# LANGUAGE OverloadedStrings #-}

module Player
  ( Player(..)
  , new
  , hasName
  ) where

import Client (Client)
import Data.Text (Text)
import Prelude hiding (id)

data Player
  = Player
      { id :: Int
      , name :: Text
      , score :: Int
      , client :: Client
      }

new :: Text -> Client -> Player
new name client =
  Player
    { id = 0
    , name = name
    , score = 0
    , client = client
    }

hasName :: Text -> Player -> Bool
hasName name (Player { name = n }) =
  name == n
