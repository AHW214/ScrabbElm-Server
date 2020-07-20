module Scrabble.Log
  ( Log (..)
  , Logger (..)
  , LogLevel (..)
  , runLogger
  ) where


--------------------------------------------------------------------------------
import           Control.Monad  (forever, (<=<))
import           Data.Text      (Text)
import           TextShow       (FromStringShow (..), TextShow (..))

import           Scrabble.Types (Context (..), Log (..), LogLevel (..),
                                 Talk (..))

import qualified Data.Text.IO   as Text
import qualified Data.Time      as Time


--------------------------------------------------------------------------------
class Monad m => Logger m where
  logOnThread :: LogLevel -> Text -> m ()

  logWhen :: LogLevel -> Context -> Text -> m ()

  logError :: Context -> Text -> m ()

  logWarning :: Context -> Text -> m ()

  logInfo :: Context -> Text -> m ()

  logDebug :: Context -> Text -> m ()

  printWhen :: TextShow a => LogLevel -> Context -> a -> m ()

  printError :: TextShow a => Context -> a -> m ()

  printWarning :: TextShow a => Context -> a -> m ()

  printInfo :: TextShow a => Context -> a -> m ()

  printDebug :: TextShow a => Context -> a -> m ()


--------------------------------------------------------------------------------
instance Logger IO where
  logOnThread level text = do
    currentTime <- Time.getCurrentTime
    Text.putStrLn $ showt (FromStringShow currentTime)
                  <> " |" <> levelToTag level
                  <> "| " <> text

  logWhen level Context { contextLoggerQueue, contextLogLevel } =
    if level >= contextLogLevel then
      emitIO contextLoggerQueue . Log level
    else
      pure . const ()

  logError =
    logWhen LogError

  logWarning =
    logWhen LogWarning

  logInfo =
    logWhen LogInfo

  logDebug =
    logWhen LogDebug

  printWhen level context =
    logWhen level context . showt

  printError =
    printWhen LogError

  printWarning =
    printWhen LogWarning

  printInfo =
    printWhen LogInfo

  printDebug =
    printWhen LogDebug


--------------------------------------------------------------------------------
runLogger :: Context -> IO ()
runLogger = forever . logQueue
  where
    logQueue :: Context -> IO ()
    logQueue =
      writeLog <=< receiveIO . contextLoggerQueue

    writeLog :: Log -> IO ()
    writeLog (Log level text) =
      logOnThread level text


--------------------------------------------------------------------------------
levelToTag :: LogLevel -> Text
levelToTag = \case
  LogError   -> "ERROR"
  LogWarning -> "WARN"
  LogInfo    -> "INFO"
  LogDebug   -> "DEBUG"
