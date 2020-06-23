{-# LANGUAGE DeriveGeneric #-}

module Scrabble.Config
  ( Config (..)
  , load
  , path
  , placeholder
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson                     (FromJSON)
import           Data.Text                      (Text)
import           Data.Time.Clock                (NominalDiffTime)
import           GHC.Generics                   (Generic)
import           Network.Wai.Handler.Warp       (Port)

import qualified Data.Aeson                     as JSON


--------------------------------------------------------------------------------
data Config = Config
  { configAuthKey        :: Text
  , configPendingTimeout :: NominalDiffTime
  , configPort           :: Maybe Port
  } deriving Generic


--------------------------------------------------------------------------------
instance FromJSON Config


--------------------------------------------------------------------------------
placeholder :: Config
placeholder = Config
  { configAuthKey        = "PLACEHOLDER AUTH KEY"
  , configPendingTimeout = 5
  , configPort           = Just 3000
  }


--------------------------------------------------------------------------------
load :: FilePath -> IO (Either String Config)
load = JSON.eitherDecodeFileStrict'


--------------------------------------------------------------------------------
path :: FilePath
path = "config.json"
