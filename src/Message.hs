module Message
  ( ClientMessage (..)
  , eitherDecode
  , listRooms
  , removeRoom
  , updateRoom
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow        (left)
import           Data.Aeson           (FromJSON, Value, (.=), (.:))
import qualified Data.Aeson           as JSON
import qualified Data.ByteString      as BSS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as T


--------------------------------------------------------------------------------
import           Room   (Room)
import           Server (Server (..))


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
listRooms Server { rooms } =
  withMessage "listRooms"
    $ JSON.object [ "rooms" .= Map.elems rooms ]
