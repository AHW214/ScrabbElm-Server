-- | Entry point for the program.
module Main
  ( main,
  )
where

import CLI (Options (..), readOptions)
import RIO
import RIO.Process (mkDefaultProcessContext)
import Scrabble.App (App (..))
import Scrabble.Logger (runLoggerThread)
import Scrabble.Run (run)

-- | Run the program.
main :: IO ()
main = do
  (Options {optionsLogLevel, optionsPort}, _) <- readOptions

  -- logOptions <- createLogOptions optionsLogLevel
  (logFunc, _) <- runLoggerThread optionsLogLevel
  processContext <- mkDefaultProcessContext

  let app =
        App
          { appLogFunc = logFunc,
            appProcessContext = processContext
          }
   in runRIO app $ run optionsPort

-- | Create log options with the given minimum log level.
createLogOptions :: MonadIO m => LogLevel -> m LogOptions
createLogOptions logLevel =
  setLogMinLevel logLevel
    . setLogUseLoc False
    <$> logOptionsHandle stdout True
