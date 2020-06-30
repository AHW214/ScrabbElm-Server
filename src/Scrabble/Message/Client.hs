module Scrabble.Message.Client
  ( ClientMessage (..)
  , eitherDecode
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow   (left)
import           Data.Aeson      (FromJSON, (.:))
import           Data.ByteString (ByteString)
import           Data.Text       (Text)

import qualified Data.Aeson      as JSON
import qualified Data.Text       as Text


--------------------------------------------------------------------------------
data ClientMessage
  = ClientNewRoom Text Int
  | ClientJoinRoom Text Text
  | ClientLeaveRoom


--------------------------------------------------------------------------------
instance FromJSON ClientMessage where
  parseJSON = JSON.withObject "ClientMessage" $ \v -> do
    messageType <- v .: "messageType"
    messageData <- v .: "messageData"

    let withNone = pure

    let withTwo msg p1 p2 =
          msg <$> messageData .: p1
              <*> messageData .: p2

    case messageType of
      "newRoom" ->
        withTwo ClientNewRoom "roomName" "roomCapacity"

      "joinRoom" ->
        withTwo ClientJoinRoom "playerName" "roomName"

      "leaveRoom" ->
        withNone ClientLeaveRoom

      msgType ->
        fail $ "Invalid message type '" ++ msgType ++ "'"


--------------------------------------------------------------------------------
eitherDecode :: ByteString -> Either Text ClientMessage
eitherDecode = left Text.pack . JSON.eitherDecodeStrict'
