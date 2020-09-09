module Scrabble.Logger
  ( runLoggerThread,
  )
where

import Data.ByteString.Builder.Extra (flush)
import RIO
import RIO.Time

type LoggerQueue = TBQueue Builder

runLoggerThread :: LogLevel -> IO (LogFunc, Async ())
runLoggerThread logLevel = do
  loggerQueue <- newTBQueueIO 256

  thread <- async $
    forever $ do
      msg <- atomically $ readTBQueue loggerQueue
      hPutBuilder stdout $ msg <> flush

  pure
    ( concurrentLogFunc
        loggerQueue
        logLevel,
      thread
    )

concurrentLogFunc :: LoggerQueue -> LogLevel -> LogFunc
concurrentLogFunc loggerQueue logLevel = mkLogFunc $ \_ _ level ->
  when (level >= logLevel) . tellLogger <=< formatMessage level
  where
    tellLogger :: Utf8Builder -> IO ()
    tellLogger =
      atomically
        . writeTBQueue loggerQueue
        . getUtf8Builder

formatMessage :: LogLevel -> Utf8Builder -> IO Utf8Builder
formatMessage logLevel message = do
  timestamp <- getTimestamp
  pure $ timestamp <> " [" <> levelName logLevel <> "] " <> message

getTimestamp :: IO Utf8Builder
getTimestamp = fromString . format <$> getZonedTime
  where
    format :: FormatTime t => t -> String
    format = take timestampLength . formatTime defaultTimeLocale "%F %T.%q"

timestampLength :: Int
timestampLength =
  length $ formatTime defaultTimeLocale "%F %T.000000" (UTCTime (ModifiedJulianDay 0) 0)

levelName :: LogLevel -> Utf8Builder
levelName = \case
  LevelDebug -> "DEBUG"
  LevelInfo -> "INFO"
  LevelWarn -> "WARN"
  LevelError -> "ERROR"
  LevelOther level -> display level
