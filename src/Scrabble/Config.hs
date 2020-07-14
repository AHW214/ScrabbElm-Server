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

import           Scrabble.Authentication  (Secret)
import           Scrabble.Types           (LogLevel (..))

import qualified Data.Aeson               as JSON
import qualified Data.Text                as Text

import qualified Scrabble.Authentication  as Auth


--------------------------------------------------------------------------------
data Config = Config
  { configAuthSecret     :: Secret
  , configLogLevel       :: LogLevel
  , configPendingTimeout :: NominalDiffTime
  , configPort           :: Port
  }


--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON = JSON.withObject "Config" $ \v -> Config
    <$> v .:  "configAuthSecret"
    <*> v .:? "configLogLevel"       .!= configLogLevel placeholder
    <*> v .:  "configPendingTimeout"
    <*> v .:? "configPort"           .!= configPort placeholder


--------------------------------------------------------------------------------
placeholder :: Config
placeholder = Config
  { configAuthSecret     = Auth.createSecret "PLACEHOLDER AUTH KEY"
  , configLogLevel       = LogWarning
  , configPendingTimeout = 5
  , configPort           = 3000
  }


--------------------------------------------------------------------------------
decode :: ByteString -> Either Text Config
decode = left Text.pack . JSON.eitherDecodeStrict'


--------------------------------------------------------------------------------
path :: FilePath
path = "config.json"
