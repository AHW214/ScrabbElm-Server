-- | Clients of the web server.
module Scrabble.Client
  ( Client,
    ID,
  )
where

import RIO
import Scrabble.Common (ID (..))

-- | A client of the server (just a placeholder for now).
data Client

instance Display (ID Client) where
  textDisplay (ID clientId) =
    "client #" <> clientId
