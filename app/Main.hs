-- | Entry point for the program.
module Main
  ( main,
  )
where

import CLI (Options (..), readOptions)
import RIO
import RIO.Process (mkDefaultProcessContext)
import Scrabble.App (App (..))
import Scrabble.Logger (LoggerOptions (..), runLoggerThread)
import Scrabble.Run (run)

-- | Run the program.
main :: IO ()
main = do
  (Options {optionsLogLevel, optionsPort}, _) <- readOptions

  let loggerOptions =
        LoggerOptions
          { loggerMinLevel = optionsLogLevel,
            loggerQueueCapacity = 256,
            loggerUseColor = True
          }

  (logFunc, _) <- runLoggerThread loggerOptions
  processContext <- mkDefaultProcessContext

  let app =
        App
          { appLogFunc = logFunc,
            appProcessContext = processContext
          }
   in runRIO app $ run optionsPort
