{-# LANGUAGE DeriveGeneric #-}

module Scrabble.Message.Server
  ( ServerMessage (..)
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson               (ToJSON)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)

import           Scrabble.Room            (Room, RoomPreview)

import qualified Data.Aeson               as JSON

import qualified Scrabble.Message.Options as Message


--------------------------------------------------------------------------------
data ServerMessage
  = ServerNewRoom RoomPreview
  | ServerUpdateRoom Room
  | ServerRemoveRoom Text
  | ServerListRooms [ RoomPreview ]
  | ServerJoinRoom Room
  | ServerLeaveRoom Text
  | ServerError Text -- use adt
  deriving Generic


--------------------------------------------------------------------------------
instance ToJSON ServerMessage where
  toEncoding = JSON.genericToEncoding Message.jsonOptions
