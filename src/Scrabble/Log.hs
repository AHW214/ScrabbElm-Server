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
  logAs :: LogLevel -> Logger m

  logError :: Logger m

  logWarning :: Logger m

  logInfo :: Logger m

  logDebug :: Logger m

  printAs :: TextShow a => LogLevel -> Printer m a

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
  logAs level Server { serverLogLevel } =
    if level >= serverLogLevel then
      Text.putStrLn . (LogLevel.toTag level <>) . (": " <>)
    else
      pure . const ()

  logError =
    logAs LogError

  logWarning =
    logAs LogWarning

  logInfo =
    logAs LogInfo

  logDebug =
    logAs LogDebug

  printAs level server =
    logAs level server . showt

  printError =
    printAs LogError

  printWarning =
    printAs LogWarning

  printInfo =
    printAs LogInfo

  printDebug =
    printAs LogDebug
