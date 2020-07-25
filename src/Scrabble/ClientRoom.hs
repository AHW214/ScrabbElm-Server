module Scrabble.ClientRoom
  ( Client (..)
  , Room (..)
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (TVar)
import           Data.Aeson             (ToJSON (toEncoding, toJSON), (.=))
import           Data.Map               (Map)
import           Network.WebSockets     (Connection)

import           Scrabble.Common        (ID)
import           Scrabble.Player        (Player)

import qualified Data.Aeson             as JSON


--------------------------------------------------------------------------------
data Room = Room
  { roomCapacity :: Int
  , roomId       :: ID Room
  , roomPlayers  :: Map (ID Client) Player
  }


--------------------------------------------------------------------------------
instance ToJSON Room where
  toJSON Room { roomCapacity, roomId, roomPlayers } =
    JSON.object
      [ "roomCapacity"  .= roomCapacity
      , "roomId"        .= roomId
      , "roomOccupancy" .= length roomPlayers
      ]

  toEncoding Room { roomCapacity, roomId, roomPlayers } =
    JSON.pairs
      $  "roomCapacity"  .= roomCapacity
      <> "roomId"        .= roomId
      <> "roomOccupancy" .= length roomPlayers


--------------------------------------------------------------------------------
data Client = Client
  { clientConnection :: Connection
  , clientId         :: ID Client
  , clientRoom       :: Maybe (TVar Room)
  }
