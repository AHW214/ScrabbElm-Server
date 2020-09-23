{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config (..),
    defaultConfig,
    readConfig,
  )
where

import qualified Configuration.Dotenv as Env
import Control.Monad.Except (ExceptT (..), withExceptT)
import Control.Monad.Extra (whenJust)
import RIO
import qualified RIO.Text as Text
import Scrabble.Authentication.Token (Secret)
import Scrabble.Common
-- TO-FIX: eh
import Scrabble.Logger ()
import System.Envy
import System.IO.Error (isDoesNotExistError)

data Config = Config
  { configAuthExpireMilliseconds :: !Integer,
    configAuthTokenSecret :: !(Maybe Secret),
    configMinLogLevel :: !LogLevel,
    configServerPort :: !Int
  }
  deriving (Generic)

data ConfigError
  = ConfigCannotDecode Text
  | ConfigCannotParse SomeException
  | ConfigCannotSet Text
  | ConfigDoesNotExist FilePath
  | ConfigIOException IOException -- TODO: rename or something idk

instance Display ConfigError where
  display = \case
    ConfigCannotDecode reason ->
      "Cannot decode env file: "
        <> display reason
    ConfigCannotParse ex ->
      "Cannot parse env file: "
        <> displayShow ex
    ConfigCannotSet reason ->
      "Cannot set loaded env vars: "
        <> display reason
    ConfigDoesNotExist filePath ->
      "Env file "
        <> displayShow filePath
        <> " does not exist"
    ConfigIOException ex ->
      "Spooky IO exception: "
        <> displayShow ex

readConfig ::
  forall m.
  MonadUnliftIO m =>
  Maybe FilePath ->
  ExceptT ConfigError m Config
readConfig mConfigFile = do
  whenJust mConfigFile loadConfigFile
  decodeConfig
  where
    decodeConfig :: ExceptT ConfigError m Config
    decodeConfig =
      withExceptT (ConfigCannotDecode . Text.pack) $
        ExceptT $ configFromEnv $ Just defaultConfig

loadConfigFile ::
  forall m.
  MonadUnliftIO m =>
  FilePath ->
  ExceptT ConfigError m ()
loadConfigFile =
  setEnvVars <=< parseConfigFile
  where
    setEnvVars :: EnvList a -> ExceptT ConfigError m ()
    setEnvVars =
      withExceptT (ConfigCannotSet . Text.pack)
        . ExceptT
        . liftIO
        . setEnvironment

parseConfigFile ::
  forall m a.
  MonadUnliftIO m =>
  FilePath ->
  ExceptT ConfigError m (EnvList a)
parseConfigFile = ExceptT . tryParse
  where
    tryParse :: FilePath -> m (Either ConfigError (EnvList a))
    tryParse filePath =
      tries
        (pairsToEnvList <$> Env.parseFile filePath)
        [ Try
            ( \(ex :: IOException) ->
                if isDoesNotExistError ex
                  then ConfigDoesNotExist filePath
                  else ConfigIOException ex
            ),
          Try ConfigCannotParse
        ]

    pairsToEnvList :: [(String, String)] -> EnvList a
    pairsToEnvList =
      makeEnv . fmap (uncurry EnvVar)

configFromEnv :: MonadIO m => Maybe Config -> m (Either String Config)
configFromEnv =
  liftIO . runEnv . gFromEnvCustom parseOptions
  where
    parseOptions :: Option
    parseOptions =
      defOption
        { dropPrefixCount = Text.length "config"
        }

defaultConfig :: Config
defaultConfig =
  Config
    { configAuthExpireMilliseconds = 5000,
      configAuthTokenSecret = Nothing,
      configMinLogLevel = LevelInfo,
      configServerPort = 3000
    }
