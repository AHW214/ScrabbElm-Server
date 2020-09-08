module Scrabble.Run (run) where

import RIO (RIO, logInfo)
import Scrabble.App (App)

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
