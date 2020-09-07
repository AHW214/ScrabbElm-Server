module Scrabble.Run (run) where

import Scrabble.Import (App, RIO, logInfo)

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
