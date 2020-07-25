module Scrabble.Client
  ( Client (..)
  , getRoom
  , inRoom
  , joinRoom
  , leaveRoom
  , new
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (TVar)
import           Network.WebSockets     (Connection)

import           Scrabble.ClientRoom    (Client (..), Room)
import           Scrabble.Common        (ID)

import qualified Data.Maybe             as Maybe


--------------------------------------------------------------------------------
new :: ID Client -> Connection -> Client
new clientId clientConnection = Client
  { clientConnection
  , clientId
  , clientRoom = Nothing
  }


--------------------------------------------------------------------------------
inRoom :: Client -> Bool
inRoom = Maybe.isJust . getRoom


--------------------------------------------------------------------------------
getRoom :: Client -> Maybe (TVar Room)
getRoom = clientRoom


--------------------------------------------------------------------------------
joinRoom :: TVar Room -> Client -> Client
joinRoom tRoom client = client
  { clientRoom = Just tRoom
  }


--------------------------------------------------------------------------------
leaveRoom :: Client -> Client
leaveRoom client = client
  { clientRoom = Nothing
  }
