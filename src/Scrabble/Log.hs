module Scrabble.Log
  ( Log (..)
  , Logger (..)
  , LogLevel (..)
  , runLogger
  ) where


--------------------------------------------------------------------------------
import           Control.Monad          (forever, (<=<))
import           Data.Text              (Text)
import           TextShow               (TextShow (..))

import           Scrabble.Types         (Context (..), Log (..), LogLevel (..))

import qualified Data.Text.IO           as Text
import qualified Control.Concurrent.STM as STM


--------------------------------------------------------------------------------
class Monad m => Logger m where
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
  logWhen level Context { contextLoggerQueue, contextLogLevel } =
    if level >= contextLogLevel then
      STM.atomically . STM.writeTBQueue contextLoggerQueue . Log level
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
      writeLog <=< STM.atomically . STM.readTBQueue . contextLoggerQueue

    writeLog :: Log -> IO ()
    writeLog (Log level text) =
      Text.putStrLn $ levelToTag level <> ": " <> text


--------------------------------------------------------------------------------
levelToTag :: LogLevel -> Text
levelToTag = \case
  LogError   -> "ERROR"
  LogWarning -> "WARN"
  LogInfo    -> "INFO"
  LogDebug   -> "DEBUG"
