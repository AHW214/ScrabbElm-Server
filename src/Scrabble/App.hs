module Scrabble.App
  ( App (..),
    HasLoggerQueue (..),
  )
where

import RIO
import RIO.Process (HasProcessContext (..), ProcessContext)
import Scrabble.Logger

data App = App
  { appLogFunc :: !LogFunc,
    appLoggerQueue :: !LoggerQueue,
    appProcessContext :: !ProcessContext
  }

class HasLoggerQueue env where
  loggerQueueL :: Lens' env LoggerQueue

instance HasLoggerQueue App where
  loggerQueueL = lens appLoggerQueue (\x y -> x {appLoggerQueue = y})

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
