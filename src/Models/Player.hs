{-# LANGUAGE OverloadedStrings #-}

module Models.Player
  ( Player(..)
  , empty
  , new
  , hasName
  ) where

import Data.Text (Text)
import Prelude hiding (id)

data Player
  = Player
      { id :: Int
      , name :: Text
      , score :: Int
      }

empty :: Player
empty =
  Player
    { id = 0
    , name = ""
    , score = 0
    }

new :: Text -> Player
new name = empty { name = name }

hasName :: Text -> Player -> Bool
hasName name (Player { name = n }) =
  name == n
