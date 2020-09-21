{-# LANGUAGE DeriveGeneric #-}

module Config
  ( loadConfig,
  )
where

import qualified Configuration.Dotenv as Env
import Control.Monad.Except (ExceptT (..), withExceptT)
import RIO
import qualified RIO.Text as Text
import System.Envy

data Config = Config
  { configAuthExpireMilliseconds :: !Integer,
    configAuthTokenSecret :: !Text,
    configMinLogLevel :: !Text,
    configServerPort :: !Int
  }
  deriving (Show, Generic)

instance FromEnv Config where
  fromEnv =
    gFromEnvCustom
      defOption
        { dropPrefixCount = Text.length "config"
        }

data ConfigError
  = ConfigCannotDecode Text
  | ConfigCannotParse SomeException -- TODO: refine exception types
  | ConfigCannotSet Text

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

loadConfig ::
  forall m.
  MonadUnliftIO m =>
  FilePath ->
  ExceptT ConfigError m Config
loadConfig =
  const decodeConfig
    <=< setEnvVars
    <=< parseEnvFile
  where
    decodeConfig :: ExceptT ConfigError m Config
    decodeConfig =
      withExceptT (ConfigCannotDecode . Text.pack) $
        ExceptT $
          liftIO decodeEnv

    setEnvVars :: EnvList a -> ExceptT ConfigError m ()
    setEnvVars =
      withExceptT (ConfigCannotSet . Text.pack)
        . ExceptT
        . liftIO
        . setEnvironment

    parseEnvFile :: FilePath -> ExceptT ConfigError m (EnvList a)
    parseEnvFile =
      withExceptT ConfigCannotParse
        . fmap pairsToEnvList
        . ExceptT
        . try
        . Env.parseFile

    pairsToEnvList :: [(String, String)] -> EnvList a
    pairsToEnvList =
      makeEnv . fmap (uncurry EnvVar)
