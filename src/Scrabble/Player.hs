module Scrabble.Player
  ( Player (..)
  , hasName
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson      (ToJSON, (.=))
import           Data.Text       (Text)

import           Scrabble.Client (Client)

import qualified Data.Aeson      as JSON


--------------------------------------------------------------------------------
data Player = Player
  { playerClient :: Client
  , playerName   :: Text
  , playerScore  :: Int
  }


--------------------------------------------------------------------------------
instance Eq Player where
  Player { playerName = nameOne } == Player { playerName = nameTwo } =
    nameOne == nameTwo


--------------------------------------------------------------------------------
instance ToJSON Player where
  toJSON Player { playerName, playerScore } =
    JSON.object
      [ "playerName"  .= playerName
      , "playerScore" .= playerScore
      ]

  toEncoding Player { playerName, playerScore } =
    JSON.pairs
      $  "playerName"  .= playerName
      <> "playerScore" .= playerScore


--------------------------------------------------------------------------------
new :: Text -> Client -> Player
new name client = Player
  { playerClient = client
  , playerName   = name
  , playerScore  = 0
  }


--------------------------------------------------------------------------------
hasName :: Text -> Player -> Bool
hasName n = (n ==) . playerName
