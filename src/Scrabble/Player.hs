module Scrabble.Player
  ( Player (..)
  , hasId
  , new
  ) where


--------------------------------------------------------------------------------
import           Scrabble.Common (ID)


--------------------------------------------------------------------------------
data Player = Player
  { playerId    :: ID Player
  , playerScore :: Int
  }


--------------------------------------------------------------------------------
new :: ID Player -> Player
new playerId = Player
  { playerId
  , playerScore = 0
  }


--------------------------------------------------------------------------------
hasId :: ID Player -> Player -> Bool
hasId pId = (pId ==) . playerId
