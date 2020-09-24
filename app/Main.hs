-- | Entry point for the program.
module Main
  ( main,
  )
where

import CLI (Options (..), readOptions)
import Config (Config (..), readConfig)
import Control.Monad.Except (runExceptT) -- TODO: Integrate into loadConfig ?
import RIO
import Scrabble.App
import Scrabble.Authentication.Client (ClientAuth (..))
import qualified Scrabble.Authentication.Client as Auth
import Scrabble.Logger (LoggerOptions (..), runLoggerThread)
import Scrabble.Run (run)

-- | Run the program.
main :: IO ()
main = do
  ( Options
      { optionsColor,
        optionsConfigFile,
        optionsPort,
        optionsQuiet,
        optionsVerbose,
        optionsVerbosity
      },
    _
    ) <-
    readOptions

  runExceptT (readConfig optionsConfigFile) >>= \case
    Left err -> do
      hPutBuilder stderr $ getUtf8Builder $ display err -- TODO
      exitFailure
    Right config -> do
      let Config
            { configAuthExpireMilliseconds,
              configAuthTokenSecret,
              configMinLogLevel,
              configServerPort
            } = config

          serverPort =
            optionsPort `orDefault` configServerPort

          logLevel
            | optionsQuiet = LevelError
            | optionsVerbose = LevelDebug
            | otherwise = optionsVerbosity `orDefault` configMinLogLevel

          loggerOptions =
            LoggerOptions
              { loggerHandle = stdout,
                loggerMinLevel = logLevel,
                loggerQueueCapacity = 256,
                loggerUseColor = optionsColor
              }

      clientCache <- atomically $ Auth.createCache
      (logFunc, _) <- runLoggerThread loggerOptions

      let app =
            App
              { appClientAuth =
                  ClientAuth
                    { authClientCache = clientCache,
                      authExpireMilliseconds = configAuthExpireMilliseconds,
                      authTokenSecret = configAuthTokenSecret
                    },
                appLogFunc = logFunc
              }
       in runRIO app $ run serverPort

orDefault :: Maybe a -> a -> a
orDefault = flip fromMaybe
