module Scrabble.Player
  ( Player (..)
  , hasName
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Text      (Text)

import           Scrabble.Types (Player (..))


--------------------------------------------------------------------------------
new :: Text -> Player
new name = Player
  { playerName  = name
  , playerScore = 0
  }


--------------------------------------------------------------------------------
hasName :: Text -> Player -> Bool
hasName name = (name ==) . playerName
