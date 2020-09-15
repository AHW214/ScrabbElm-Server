module Scrabble.Client
  ( Client,
    ID,
  )
where

import RIO
import Scrabble.Common (ID (..))

data Client

instance Display (ID Client) where
  textDisplay (ID clientId) =
    "client #" <> clientId
