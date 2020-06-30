module Scrabble.Message.Server
  ( ServerMessage (..)
  , encode
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson           (Value, (.=))
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)

import           Scrabble.Room        (Room (..))
import           Scrabble.Server      (Server (..))

import qualified Data.Aeson           as JSON
import qualified Data.Map             as Map


--------------------------------------------------------------------------------
data ServerMessage
  = ServerNewRoom Room
  | ServerUpdateRoom Room
  | ServerRemoveRoom Room
  | ServerListRooms Server
  | ServerJoinRoom Room
  | ServerLeaveRoom Room
  | ServerError Text -- use adt


--------------------------------------------------------------------------------
encode :: ServerMessage -> ByteString
encode = \case
  ServerNewRoom room ->
    withMessage "newRoom" $ JSON.object [ "room" .= room ]

  ServerUpdateRoom room ->
    withMessage "updateRoom" $ JSON.object [ "room" .= room ]

  ServerRemoveRoom Room { roomName } ->
    withMessage "removeRoom" $ JSON.object [ "roomName" .= roomName ]

  ServerListRooms Server { serverRooms } ->
    withMessage "listRooms" $ JSON.object [ "rooms" .= Map.elems serverRooms ]

  ServerJoinRoom room ->
    withMessage "joinRoom" $ JSON.object [ "room" .= room ]

  ServerLeaveRoom Room { roomName } ->
    withMessage "leaveRoom" $ JSON.object [ "roomName" .= roomName ]


--------------------------------------------------------------------------------
withMessage :: Text -> Value -> ByteString
withMessage messageType messageData =
  JSON.encode $ JSON.object
    [ "messageType" .= messageType
    , "messageData" .= messageData
    ]
