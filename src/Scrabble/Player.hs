module Scrabble.Player
  ( Player (..)
  , hasName
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson         (ToJSON, (.=))
import           Data.Text          (Text)
import           Network.WebSockets (Connection)

import           Scrabble.Tickets   (Ticket)

import qualified Data.Aeson         as JSON


--------------------------------------------------------------------------------
data Player = Player
  { playerClient :: ( Ticket, Connection )
  , playerId     :: Int
  , playerName   :: Text
  , playerScore  :: Int
  }


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
new :: Text -> ( Ticket, Connection ) -> Player
new name client = Player
  { playerClient = client
  , playerId     = 0 -- TODO
  , playerName   = name
  , playerScore  = 0
  }


--------------------------------------------------------------------------------
hasName :: Text -> Player -> Bool
hasName n = (n ==) . playerName
