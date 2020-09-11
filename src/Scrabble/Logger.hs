module Scrabble.Logger
  ( LogColor (..),
    LoggerOptions (..),
    logLevelNameAndColor,
    runLoggerThread,
  )
where

import Data.ByteString.Builder.Extra (flush)
import RIO
import RIO.Time
import System.Console.ANSI

data LoggerOptions = LoggerOptions
  { loggerHandle :: !Handle,
    loggerMinLevel :: !LogLevel,
    loggerQueueCapacity :: !Natural,
    loggerUseColor :: !LogColor
  }

data LogColor
  = AlwaysColor
  | NeverColor
  | AutoColor

type LoggerQueue = TBQueue Builder

runLoggerThread :: LoggerOptions -> IO (LogFunc, Async ())
runLoggerThread loggerOptions = do
  let LoggerOptions
        { loggerHandle,
          loggerQueueCapacity
        } = loggerOptions

  loggerQueue <- newTBQueueIO loggerQueueCapacity

  thread <- async $
    forever $ do
      msg <- atomically $ readTBQueue loggerQueue
      hPutBuilder loggerHandle $ msg <> flush

  logFunc <-
    concurrentLogFunc
      loggerQueue
      loggerOptions

  pure (logFunc, thread)

concurrentLogFunc :: LoggerQueue -> LoggerOptions -> IO LogFunc
concurrentLogFunc loggerQueue loggerOptions = do
  let LoggerOptions
        { loggerHandle,
          loggerMinLevel,
          loggerUseColor
        } = loggerOptions

  useColor <-
    case loggerUseColor of
      AutoColor ->
        hIsTerminalDevice loggerHandle
      AlwaysColor ->
        pure True
      NeverColor ->
        pure False

  let ansi =
        if useColor
          then fromString . setSGRCode
          else const ""

  pure $
    mkLogFunc $ \_ _ level ->
      when (level >= loggerMinLevel)
        . atomically
        . writeTBQueue loggerQueue
        . getUtf8Builder
        <=< formatMessage ansi level

formatMessage :: ([SGR] -> Utf8Builder) -> LogLevel -> Utf8Builder -> IO Utf8Builder
formatMessage ansi logLevel message = do
  timestamp <- getTimestamp

  let timestampColor = ansi [SetColor Foreground Vivid Black]
  let (levelName, levelColor) = logLevelNameAndColor logLevel
  let levelHeader = "[" <> levelName <> "]"
  let color = ansi [SetColor Foreground Dull levelColor]
  let reset = ansi [Reset]

  pure $
    timestampColor
      <> timestamp
      <> reset
      <> " "
      <> color
      <> levelHeader
      <> reset
      <> " "
      <> message
      <> "\n"

getTimestamp :: IO Utf8Builder
getTimestamp = fromString . format <$> getZonedTime
  where
    format :: FormatTime t => t -> String
    format = take timestampLength . formatTime defaultTimeLocale "%F %T.%q"

timestampLength :: Int
timestampLength =
  length $ formatTime defaultTimeLocale "%F %T.000000" (UTCTime (ModifiedJulianDay 0) 0)

logLevelNameAndColor :: LogLevel -> (Utf8Builder, Color)
logLevelNameAndColor = \case
  LevelDebug -> ("DEBUG", Green)
  LevelInfo -> ("INFO", Blue)
  LevelWarn -> ("WARN", Yellow)
  LevelError -> ("ERROR", Red)
  LevelOther level -> (display level, Magenta)
