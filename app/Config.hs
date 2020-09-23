{-# LANGUAGE DeriveGeneric #-}

module Config
  ( loadConfig,
  )
where

import qualified Configuration.Dotenv as Env
import Control.Monad.Except (ExceptT (..), withExceptT)
import RIO
import qualified RIO.Text as Text
import Scrabble.Common
import System.Envy
import System.IO.Error (isDoesNotExistError)

data Config = Config
  { configAuthExpireMilliseconds :: !Integer,
    configAuthTokenSecret :: !(Maybe Text),
    configMinLogLevel :: !Text,
    configServerPort :: !Int
  }
  deriving (Generic, Show)

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
    ConfigCannotParse excp ->
      "Cannot parse env file: "
        <> displayShow excp
    ConfigCannotSet reason ->
      "Cannot set loaded env vars: "
        <> display reason
    ConfigDoesNotExist filePath ->
      "Env file "
        <> displayShow filePath
        <> " does not exist"
    ConfigIOException excp ->
      "Spooky IO exception: "
        <> displayShow excp

loadConfig ::
  forall m.
  MonadUnliftIO m =>
  FilePath ->
  ExceptT ConfigError m Config
loadConfig filePath =
  const decodeConfig
    =<< setEnvVars
    =<< parseEnvFile
      filePath
  where
    decodeConfig :: ExceptT ConfigError m Config
    decodeConfig =
      withExceptT (ConfigCannotDecode . Text.pack) $
        ExceptT $ configFromEnv $ Just defaultConfig

    setEnvVars :: EnvList a -> ExceptT ConfigError m ()
    setEnvVars =
      withExceptT (ConfigCannotSet . Text.pack)
        . ExceptT
        . liftIO
        . setEnvironment

    parseEnvFile :: FilePath -> ExceptT ConfigError m (EnvList a)
    parseEnvFile =
      fmap pairsToEnvList
        . ExceptT
        . tryParse
        . Env.parseFile

    tryParse :: m a -> m (Either ConfigError a) -- TODO: clean up
    tryParse =
      flip
        tries
        [ Try
            ( \(excp :: IOException) ->
                if isDoesNotExistError excp
                  then ConfigDoesNotExist filePath
                  else ConfigIOException excp
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
      configMinLogLevel = "INFO",
      configServerPort = 3000
    }
