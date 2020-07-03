module Scrabble.Log
  ( Log (..)
  , Logger
  , LogLevel (..)
  , Printer
  ) where


--------------------------------------------------------------------------------
import           Data.Text          (Text)
import           TextShow           (TextShow (..))

import           Scrabble.Log.Level (LogLevel (..))
import           Scrabble.Server    (Server (..))

import qualified Data.Text.IO       as Text

import qualified Scrabble.Log.Level as LogLevel


--------------------------------------------------------------------------------
class Monad m => Log m where
  logAs :: LogLevel -> Text -> m ()

  logWhen :: LogLevel -> Logger m

  logError :: Logger m

  logWarning :: Logger m

  logInfo :: Logger m

  logDebug :: Logger m

  printAs :: TextShow a => LogLevel -> a -> m ()

  printWhen :: TextShow a => LogLevel -> Printer m a

  printError :: TextShow a => Printer m a

  printWarning :: TextShow a => Printer m a

  printInfo :: TextShow a => Printer m a

  printDebug :: TextShow a => Printer m a


--------------------------------------------------------------------------------
type Logger m = Printer m Text


--------------------------------------------------------------------------------
type Printer m a = Server -> a -> m ()


--------------------------------------------------------------------------------
instance Log IO where
  logAs level =
    Text.putStrLn . (LogLevel.toTag level <>) . (": " <>)

  logWhen level Server { serverLogLevel } =
    if level >= serverLogLevel then
      logAs level
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

  printAs level =
    logAs level . showt

  printWhen level server =
    logWhen level server . showt

  printError =
    printWhen LogError

  printWarning =
    printWhen LogWarning

  printInfo =
    printWhen LogInfo

  printDebug =
    printWhen LogDebug
