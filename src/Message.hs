{-# LANGUAGE OverloadedStrings #-}

module Message
  ( ClientMessage(..)
  , ServerMessage(..)
  )
  where

import Data.Aeson (ToJSON, FromJSON, (.=), (.:))
import qualified Data.Aeson as JSON
import Data.Text (Text)
import Room (Room)

data ServerMessage
  = UpdateRoom Room
  | RemoveRoom Int
  | ListRooms [ Room ]

data ClientMessage
  = NewRoom Text Int
  | JoinRoom Int Int
  | LeaveRoom Int Int

instance ToJSON ServerMessage where
  toJSON message = JSON.object $
    case message of
      UpdateRoom room ->
        [ "room" .= room ]

      RemoveRoom roomId ->
        [ "roomId" .= roomId ]

      ListRooms rooms ->
        [ "rooms" .= rooms ]

  toEncoding message = JSON.pairs $
    case message of
      UpdateRoom room ->
        "room" .= room

      RemoveRoom roomId ->
        "roomID" .= roomId

      ListRooms rooms ->
        "rooms" .= rooms


instance FromJSON ClientMessage where
  parseJSON = JSON.withObject "Message" $ \v -> do
    messageType <- v .: "messageType"

    case messageType of
      "newRoom" ->
        NewRoom <$> v .: "name" <*> v .: "capacity"

      "joinRoom" ->
        JoinRoom <$> v .: "playerId" <*> v .: "roomId"

      "leaveRoom" ->
        LeaveRoom <$> v .: "playerId" <*> v .: "roomId"

      msgType ->
        fail $ "invalid message type '" ++ msgType ++ "'"
