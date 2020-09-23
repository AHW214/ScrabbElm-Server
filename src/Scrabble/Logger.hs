-- | Concurrent logging interface.
module Scrabble.Logger
  ( ColorOption (..),
    LoggerOptions (..),
    defaultLogLevel,
    logLevelNameAndColor,
    runLoggerThread,
  )
where

import Data.ByteString.Builder.Extra (flush)
import RIO
import RIO.Time
import System.Console.ANSI

-- | Options for initializing the logger interface.
data LoggerOptions = LoggerOptions
  { -- | The handle of the stream or file that will receive log messages.
    loggerHandle :: !Handle,
    -- | The minimum level at which logs will be made.
    loggerMinLevel :: !LogLevel,
    -- | The number of messages the logger-thread queue can
    -- hold before blocking writes.
    loggerQueueCapacity :: !Natural,
    -- | Option for coloring logs.
    loggerUseColor :: !ColorOption
  }

-- | Options for coloring logs.
data ColorOption
  = -- | Always color logs.
    AlwaysColor
  | -- | Never color logs.
    NeverColor
  | -- | Atomically color logs (i.e. if the program is run from a terminal).
    AutoColor

-- | Queue for sending messages to the logger thread.
type LoggerQueue = TBQueue Builder

-- | Start the logger thread and return a custom
-- logging function for sending it messages.
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

-- | Create a concurrent logging function that writes to the logger thread.
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

-- | Format a log message with a timestamp and log level.
formatMessage ::
  -- | A function to encode ansi formatting.
  ([SGR] -> Utf8Builder) ->
  -- | The level of the log message.
  LogLevel ->
  -- | The raw log message.
  Utf8Builder ->
  -- | The formatted log message.
  IO Utf8Builder
formatMessage ansi logLevel message = do
  let (levelName, levelColor) =
        logLevelNameAndColor logLevel

      formattedLevel =
        format ("[" <> levelName <> "]") [SetColor Foreground Dull levelColor]

  timestamp <- createTimestamp

  let formattedTimestamp =
        format timestamp [SetColor Foreground Vivid Black]

  pure $
    formattedTimestamp
      <> " "
      <> formattedLevel
      <> " "
      <> message
      <> "\n"
  where
    format :: Utf8Builder -> [SGR] -> Utf8Builder
    format text sgr = ansi sgr <> text <> ansi [Reset]

-- | Create a timestamp with the current time.
createTimestamp :: IO Utf8Builder
createTimestamp = fromString . format <$> getZonedTime
  where
    format :: FormatTime t => t -> String
    format = take timestampLength . formatTime defaultTimeLocale "%F %T.%q"

-- | Fixed length for a timestamp.
timestampLength :: Int
timestampLength =
  length $ formatTime defaultTimeLocale "%F %T.000000" (UTCTime (ModifiedJulianDay 0) 0)

-- | Get the name and color for displaying a log level.
logLevelNameAndColor :: LogLevel -> (Utf8Builder, Color)
logLevelNameAndColor = \case
  LevelDebug -> ("DEBUG", Green)
  LevelInfo -> ("INFO", Blue)
  LevelWarn -> ("WARN", Yellow)
  LevelError -> ("ERROR", Red)
  LevelOther level -> (display level, Magenta)

defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo
