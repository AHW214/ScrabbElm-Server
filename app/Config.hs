module Config
  ( Config (..),
    defaultConfig,
    readConfig,
  )
where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.Extra (whenJust)
import Env
import RIO
import qualified RIO.Text as Text
import Scrabble.Authentication.Token (Secret, createFixedSizeSecret)
import Scrabble.Logger (logLevelFromName)

data Config = Config
  { configAuthExpireMilliseconds :: !Integer,
    configAuthTokenSecret :: !Secret,
    configMinLogLevel :: !LogLevel,
    configServerPort :: !Int
  }

instance FromEnv Config where
  fromEnv _ =
    Config
      <$> envOptional "AUTH_EXPIRE_MILLISECONDS" `withDefault` authExpireMilliseconds
      <*> envWith createSecret "AUTH_TOKEN_SECRET"
      <*> envOptionalWith logLevelFromName "MIN_LOG_LEVEL" `withDefault` minLogLevel
      <*> envOptional "SERVER_PORT" `withDefault` serverPort
    where
      createSecret :: String -> Either String Secret
      createSecret =
        first Text.unpack . createFixedSizeSecret . Text.pack

      Config
        { configAuthExpireMilliseconds = authExpireMilliseconds,
          configMinLogLevel = minLogLevel,
          configServerPort = serverPort
        } = defaultConfig

readConfig ::
  forall m.
  MonadUnliftIO m =>
  Maybe FilePath ->
  ExceptT EnvError m Config
readConfig mConfigFile = do
  whenJust mConfigFile loadConfigFile
  decodeConfig
  where
    decodeConfig :: ExceptT EnvError m Config
    decodeConfig =
      ExceptT decodeEnv

    loadConfigFile :: FilePath -> ExceptT EnvError m ()
    loadConfigFile =
      ExceptT . loadEnvFile

defaultConfig :: Config
defaultConfig =
  Config
    { configAuthExpireMilliseconds = 5000,
      configAuthTokenSecret = "",
      configMinLogLevel = LevelInfo,
      configServerPort = 3000
    }
