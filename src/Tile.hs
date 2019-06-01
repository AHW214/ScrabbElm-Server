module Tile
  ( Tile
  ) where

import Data.Char (ord)
import Data.Aeson as AE (ToJSON, toJSON)

data Tile
  = Blank
  | Letter Char Int

instance ToJSON Tile where
  toJSON tile =
    toJSON $
      case tile of
        Blank ->
          [ ord ' ', 0 ]
        Letter c v ->
          [ ord c, v ]
