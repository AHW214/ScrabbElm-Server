module Scrabble.Player
  ( Player (..)
  , hasId
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Text (Text)

--import         Scrabble.Common (ID)


--------------------------------------------------------------------------------
data Player = Player
  { playerId    :: Text
  , playerScore :: Int
  }


--------------------------------------------------------------------------------
new :: Text -> Player
new playerId = Player
  { playerId
  , playerScore = 0
  }


--------------------------------------------------------------------------------
hasId :: Text -> Player -> Bool
hasId pId = (pId ==) . playerId
