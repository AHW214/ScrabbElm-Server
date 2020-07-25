{-# LANGUAGE DeriveGeneric #-}

module Scrabble.Error
  ( Error (..)
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson   (ToJSON (toEncoding))
import           Data.Text    (Text)
import           GHC.Generics (Generic)

import qualified Data.Aeson   as JSON


--------------------------------------------------------------------------------
data Error
  = ClientAlreadyInRoom
  | ClientNotInRoom
  | RoomNotFound
  | RoomAlreadyExists
  | MessageInvalid Text
  deriving Generic


--------------------------------------------------------------------------------
instance ToJSON Error where
  toEncoding = JSON.genericToEncoding JSON.defaultOptions
