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
  ( Options
      { optionsColor,
        optionsPort,
        optionsQuiet,
        optionsVerbose,
        optionsVerbosity
      },
    _
    ) <-
    readOptions

  let logLevel
        | optionsQuiet = LevelError
        | optionsVerbose = LevelDebug
        | otherwise = optionsVerbosity

      loggerOptions =
        LoggerOptions
          { loggerHandle = stdout,
            loggerMinLevel = logLevel,
            loggerQueueCapacity = 256,
            loggerUseColor = optionsColor
          }

  (logFunc, _) <- runLoggerThread loggerOptions
  processContext <- mkDefaultProcessContext

  let app =
        App
          { appLogFunc = logFunc,
            appProcessContext = processContext
          }
   in runRIO app $ run optionsPort
