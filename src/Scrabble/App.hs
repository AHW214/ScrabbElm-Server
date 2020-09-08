module Scrabble.App (App (..)) where

import RIO
import RIO.Process (HasProcessContext (..), ProcessContext)

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
