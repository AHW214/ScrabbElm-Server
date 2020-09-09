module Scrabble.Logger
  ( LoggerOptions (..),
    runLoggerThread,
  )
where

import Data.ByteString.Builder.Extra (flush)
import RIO
import RIO.Time
import System.Console.ANSI

data LoggerOptions = LoggerOptions
  { loggerMinLevel :: !LogLevel,
    loggerQueueCapacity :: !Natural,
    loggerUseColor :: !Bool
  }

type LoggerQueue = TBQueue Builder

runLoggerThread :: LoggerOptions -> IO (LogFunc, Async ())
runLoggerThread loggerOptions = do
  loggerQueue <- newTBQueueIO $ loggerQueueCapacity loggerOptions

  thread <- async $
    forever $ do
      msg <- atomically $ readTBQueue loggerQueue
      hPutBuilder stdout $ msg <> flush

  pure
    ( concurrentLogFunc
        loggerQueue
        loggerOptions,
      thread
    )

concurrentLogFunc :: LoggerQueue -> LoggerOptions -> LogFunc
concurrentLogFunc loggerQueue loggerOptions =
  let LoggerOptions
        { loggerMinLevel = logLevel,
          loggerUseColor = useColor
        } = loggerOptions

      ansi =
        if useColor
          then fromString . setSGRCode
          else const ""
   in mkLogFunc $ \_ _ level ->
        when (level >= logLevel)
          . atomically
          . writeTBQueue loggerQueue
          . getUtf8Builder
          <=< formatMessage ansi logLevel

formatMessage :: ([SGR] -> Utf8Builder) -> LogLevel -> Utf8Builder -> IO Utf8Builder
formatMessage ansi logLevel message = do
  timestamp <- getTimestamp

  let timestampColor = ansi [SetColor Foreground Vivid Black]
  let (levelName, levelColor) = levelNameAndColor logLevel
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

levelNameAndColor :: LogLevel -> (Utf8Builder, Color)
levelNameAndColor = \case
  LevelDebug -> ("DEBUG", Green)
  LevelInfo -> ("INFO", Blue)
  LevelWarn -> ("WARN", Yellow)
  LevelError -> ("ERROR", Red)
  LevelOther level -> (display level, Magenta)
