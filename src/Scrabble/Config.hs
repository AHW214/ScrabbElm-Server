module Scrabble.Config
  ( Config (..)
  , decode
  , path
  , placeholder
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow            (left)
import           Data.Aeson               (FromJSON, (.:), (.:?), (.!=))
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           Data.Time.Clock          (NominalDiffTime)
import           Network.Wai.Handler.Warp (Port)

import           Scrabble.Types           (LogLevel (..), Secret)

import qualified Data.Aeson               as JSON
import qualified Data.Text                as Text


--------------------------------------------------------------------------------
data Config = Config
  { configAuthSecret    :: Secret
  , configLogLevel      :: LogLevel
  , configTimeoutLength :: NominalDiffTime
  , configPort          :: Port
  }


--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON = JSON.withObject "Config" $ \v -> Config
    <$> v .:  "configAuthSecret"
    <*> v .:? "configLogLevel"       .!= configLogLevel placeholder
    <*> v .:  "configTimeoutLength"
    <*> v .:? "configPort"           .!= configPort placeholder


--------------------------------------------------------------------------------
placeholder :: Config
placeholder = Config
  { configAuthSecret    = "PLACEHOLDER AUTH KEY"
  , configLogLevel      = LogWarning
  , configTimeoutLength = 5
  , configPort          = 3000
  }


--------------------------------------------------------------------------------
decode :: ByteString -> Either Text Config
decode = left Text.pack . JSON.eitherDecodeStrict'


--------------------------------------------------------------------------------
path :: FilePath
path = "config.json"
