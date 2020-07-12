module Scrabble.Client
  ( Client (..)
  , ClientEvent (..)
  , inRoom
  , leaveRoom
  , new
  , send
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM  (TBQueue)
import           Data.Text               (Text)
import           Network.WebSockets      (Connection)

import           Scrabble.Message.Client (ClientMessage)
-- import           Scrabble.Room           (RoomEvent (..))

import qualified Data.Maybe              as Maybe
import qualified Network.WebSockets      as WS


--------------------------------------------------------------------------------
data Client = Client
  { clientConnection :: Connection
  , clientId         :: Text
  , clientQueue      :: TBQueue ClientEvent
  , clientRoomQueue  :: Maybe (TBQueue ()) -- todo
  }


--------------------------------------------------------------------------------
data ClientEvent
  = ClientInbound ClientMessage
  | ClientOutbound Text
  | ClientRoomJoin (TBQueue ())
  | ClientRoomLeave
  | ClientDisconnect


--------------------------------------------------------------------------------
instance Eq Client where
 Client { clientId = id1 } == Client { clientId = id2 } =
   id1 == id2


--------------------------------------------------------------------------------
instance Ord Client where
  compare Client { clientId = id1 } Client { clientId = id2 } =
    compare id1 id2


--------------------------------------------------------------------------------
inRoom :: Client -> Bool
inRoom = Maybe.isJust . clientRoomQueue


--------------------------------------------------------------------------------
send :: Client -> Text -> IO () -- todo: custom message type
send Client { clientConnection } =
  WS.sendTextData clientConnection


--------------------------------------------------------------------------------
leaveRoom :: Client -> Client
leaveRoom client = client
  { clientRoomQueue = Nothing
  }


--------------------------------------------------------------------------------
new :: Connection -> Text -> TBQueue ClientEvent -> Client
new clientConnection clientId clientQueue = Client
  { clientConnection
  , clientId
  , clientQueue
  , clientRoomQueue = Nothing
  }
