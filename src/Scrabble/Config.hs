module Scrabble.Config
  ( Config (..),
    load,
    placeholder,
  )
where

--------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
import Data.Time.Clock (NominalDiffTime)
import Data.Yaml (FromJSON, (.!=), (.:), (.:?))
import Data.Yaml.Config (ignoreEnv, loadYamlSettings)
import Network.Wai.Handler.Warp (Port)
import Scrabble.Log (LogLevel (..), logOnThread)
import Scrabble.Types (Secret)
import System.Directory (doesFileExist)

--------------------------------------------------------------------------------
data Config = Config
  { configAuthSecret :: !Secret,
    configLogLevel :: !LogLevel,
    configTimeoutLength :: !NominalDiffTime,
    configPort :: !Port
  }

--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON = JSON.withObject "Config" $ \o ->
    Config
      <$> o .: "configAuthSecret"
      <*> o .:? "configLogLevel" .!= configLogLevel placeholder
      <*> o .: "configTimeoutLength"
      <*> o .:? "configPort" .!= configPort placeholder

--------------------------------------------------------------------------------
placeholder :: Config
placeholder =
  Config
    { configAuthSecret = "PLACEHOLDER AUTH KEY",
      configLogLevel = LogWarning,
      configTimeoutLength = 5,
      configPort = 3000
    }

--------------------------------------------------------------------------------
load :: IO Config
load = do
  configExists <- doesFileExist path

  if configExists
    then loadYamlSettings [path] [] ignoreEnv
    else do
      logOnThread LogWarning "Using placeholder config"
      pure placeholder

--------------------------------------------------------------------------------
path :: FilePath
path = "scrabble.yaml"
