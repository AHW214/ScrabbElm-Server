module Message
  ( ClientMessage (..)
  , listRooms
  , removeRoom
  , updateRoom
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson           (FromJSON, Value, (.=), (.:))
import qualified Data.Aeson           as JSON
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map             as Map
import           Data.Text            (Text)


--------------------------------------------------------------------------------
import           Room                 (Room)
import           Server               (Server (..))


--------------------------------------------------------------------------------
data ClientMessage
  = NewRoom Text Int
  | JoinRoom Text Text
  | LeaveRoom Text Text


--------------------------------------------------------------------------------
instance FromJSON ClientMessage where
  parseJSON = JSON.withObject "Message" $ \v -> do
    messageType <- v .: "messageType"
    messageData <- v .: "messageData"

    let withTwo msg p1 p2 =
          msg <$> messageData .: p1
              <*> messageData .: p2

    case messageType of
      "newRoom" ->
        withTwo NewRoom "name" "capacity"

      "joinRoom" ->
        withTwo JoinRoom "playerName" "roomName"

      "leaveRoom" ->
        withTwo LeaveRoom "playerName" "roomName"

      msgType ->
        fail $ "Invalid message type '" ++ msgType ++ "'"


--------------------------------------------------------------------------------
withMessage :: Text -> Value -> ByteString
withMessage messageType messageData =
  JSON.encode $ JSON.object
    [ "messageType" .= messageType
    , "messageData" .= messageData
    ]


--------------------------------------------------------------------------------
updateRoom :: Room -> ByteString
updateRoom room =
  withMessage "updateRoom"
    $ JSON.object [ "room" .= room ]


--------------------------------------------------------------------------------
removeRoom :: Text -> ByteString
removeRoom roomName =
  withMessage "removeRoom"
    $ JSON.object [ "roomName" .= roomName ]


--------------------------------------------------------------------------------
listRooms :: Server -> ByteString
listRooms Server { rooms } =
  withMessage "listRooms"
    $ JSON.object [ "rooms" .= Map.elems rooms ]
