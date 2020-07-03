{-# LANGUAGE DeriveGeneric #-}

module Scrabble.Message.Client
  ( ClientMessage (..)
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson               (FromJSON)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)

import qualified Data.Aeson               as JSON

import qualified Scrabble.Message.Options as Message


--------------------------------------------------------------------------------
data ClientMessage
  = ClientNewRoom Text Int Text
  | ClientJoinRoom Text Text
  | ClientLeaveRoom
  | ClientStartGame
  deriving Generic


--------------------------------------------------------------------------------
instance FromJSON ClientMessage where
  parseJSON = JSON.genericParseJSON Message.jsonOptions
