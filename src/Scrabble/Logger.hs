module Scrabble.Logger
  ( LoggerQueue (..),
    concurrentLogFunc,
    runLoggerThread,
  )
where

import Data.ByteString.Builder.Extra (flush)
import RIO

newtype LoggerQueue = LoggerQueue
  { unLoggerQueue :: TBQueue Utf8Builder
  }

concurrentLogFunc :: LoggerQueue -> LogLevel -> LogFunc
concurrentLogFunc (LoggerQueue queue) logLevel = mkLogFunc $ \_ _ level ->
  when (level >= logLevel) . atomically . writeTBQueue queue

runLoggerThread :: IO (LoggerQueue, Async ())
runLoggerThread = do
  queue <- newTBQueueIO 256

  thread <- async $
    forever $ do
      msg <- atomically $ readTBQueue queue
      hPutBuilder stdout (getUtf8Builder (msg <> "\n") <> flush)

  pure (LoggerQueue queue, thread)
