{-# LANGUAGE DeriveGeneric #-}

module Scrabble.Config
  ( Config (..)
  , decode
  , path
  , placeholder
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow            (left)
import           Data.Aeson               (FromJSON)
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           Data.Time.Clock          (NominalDiffTime)
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp (Port)

import qualified Data.Aeson               as JSON
import qualified Data.Text                as T


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
decode :: ByteString -> Either Text Config
decode = left T.pack . JSON.eitherDecodeStrict'


--------------------------------------------------------------------------------
path :: FilePath
path = "config.json"
