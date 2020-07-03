{-# LANGUAGE DeriveGeneric #-}

module Scrabble.Room.Preview
  ( RoomPreview (..)
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson   (ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)


--------------------------------------------------------------------------------
data RoomPreview = RoomPreview
  { roomPreviewCapacity  :: Int
  , roomPreviewInGame    :: Bool
  , roomPreviewName      :: Text
  , roomPreviewOccupancy :: Int
  } deriving Generic


--------------------------------------------------------------------------------
instance ToJSON RoomPreview
