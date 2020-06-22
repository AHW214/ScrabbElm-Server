module Scrabble.Message
  ( ClientMessage (..)
  , eitherDecode
  , joinRoom
  , leaveRoom
  , listRooms
  , newRoom
  , removeRoom
  , updateRoom
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow           (left)
import           Data.Aeson              (FromJSON, Value, (.=), (.:))
import           Data.Text               (Text)

import           Scrabble.Room           (Room)
import           Scrabble.Server         (Server (..))

import qualified Data.Aeson              as JSON
import qualified Data.ByteString         as BSS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T

import qualified Scrabble.Authentication as Auth


--------------------------------------------------------------------------------
data ClientMessage
  = Authenticate Text Auth.Plain
  | NewRoom Text Int
  | JoinRoom Text Text
  | LeaveRoom


--------------------------------------------------------------------------------
instance FromJSON ClientMessage where
  parseJSON = JSON.withObject "Message" $ \v -> do
    messageType <- v .: "messageType"
    messageData <- v .: "messageData"

    let withNone = pure

    let withTwo msg p1 p2 =
          msg <$> messageData .: p1
              <*> messageData .: p2

    case messageType of
      "authenticate" ->
        withTwo Authenticate "clientId" "clientTicket"

      "newRoom" ->
        withTwo NewRoom "roomName" "roomCapacity"

      "joinRoom" ->
        withTwo JoinRoom "playerName" "roomName"

      "leaveRoom" ->
        withNone LeaveRoom

      msgType ->
        fail $ "Invalid message type '" ++ msgType ++ "'"


--------------------------------------------------------------------------------
eitherDecode :: BSS.ByteString -> Either Text ClientMessage
eitherDecode = left T.pack . JSON.eitherDecodeStrict'


--------------------------------------------------------------------------------
withMessage :: Text -> Value -> BSL.ByteString
withMessage messageType messageData =
  JSON.encode $ JSON.object
    [ "messageType" .= messageType
    , "messageData" .= messageData
    ]


--------------------------------------------------------------------------------
newRoom :: Room -> BSL.ByteString
newRoom room =
  withMessage "newRoom"
    $ JSON.object [ "room" .= room ]


--------------------------------------------------------------------------------
updateRoom :: Room -> BSL.ByteString
updateRoom room =
  withMessage "updateRoom"
    $ JSON.object [ "room" .= room ]


--------------------------------------------------------------------------------
removeRoom :: Text -> BSL.ByteString
removeRoom roomName =
  withMessage "removeRoom"
    $ JSON.object [ "roomName" .= roomName ]


--------------------------------------------------------------------------------
listRooms :: Server -> BSL.ByteString
listRooms Server { serverRooms } =
  withMessage "listRooms"
    $ JSON.object [ "rooms" .= Map.elems serverRooms ]


--------------------------------------------------------------------------------
joinRoom :: Room -> BSL.ByteString
joinRoom room =
  withMessage "joinRoom"
    $ JSON.object [ "room" .= room ]


--------------------------------------------------------------------------------
leaveRoom :: Text -> BSL.ByteString
leaveRoom roomName =
  withMessage "leaveRoom"
    $ JSON.object [ "roomName" .= roomName ]
