{-# LANGUAGE DeriveGeneric #-}

module Scrabble.Room.View
  ( RoomView (..)
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson   (ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)


--------------------------------------------------------------------------------
data RoomView = RoomView
  { roomViewCapacity  :: Int
  , roomViewInGame    :: Bool
  , roomViewName      :: Text
  , roomViewOccupancy :: Int
  } deriving Generic


--------------------------------------------------------------------------------
instance ToJSON RoomView
