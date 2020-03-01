{-# LANGUAGE OverloadedStrings #-}

module Message
  ( ClientMessage (..)
  , listRooms
  , removeRoom
  , updateRoom
  )
  where


--------------------------------------------------------------------------------
import           Data.Aeson           (FromJSON, ToJSON, Value, (.=), (.:))
import qualified Data.Aeson           as JSON
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map             as Map
import           Data.Text            (Text)


--------------------------------------------------------------------------------
import           Room                 (Room)
import           Server               (Server (..))
import qualified Server


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

    case messageType of
      "newRoom" ->
        NewRoom
          <$> messageData .: "name"
          <*> messageData .: "capacity"

      "joinRoom" ->
        JoinRoom
          <$> messageData .: "playerName"
          <*> messageData .: "roomName"

      "leaveRoom" ->
        LeaveRoom
          <$> messageData .: "playerName"
          <*> messageData .: "roomName"

      msgType ->
        fail $ "Invalid message type '" ++ msgType ++ "'"


--------------------------------------------------------------------------------
withMessage :: Text -> Value -> ByteString
withMessage messageType messageData =
  JSON.encode $ JSON.object [ "messageType" .= messageType,  "messageData" .= messageData ]


--------------------------------------------------------------------------------
updateRoom :: Room -> ByteString
updateRoom room =
  withMessage "updateRoom" $ JSON.object [ "room" .= room ]


--------------------------------------------------------------------------------
removeRoom :: Text -> ByteString
removeRoom roomName =
  withMessage "removeRoom" $ JSON.object [ "roomName" .= roomName ]


--------------------------------------------------------------------------------
listRooms :: Server -> ByteString
listRooms server@Server { rooms = rs } =
  withMessage "listRooms" $ JSON.object [ "rooms" .= Map.elems rs ]
