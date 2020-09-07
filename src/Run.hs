module Run (run) where

import Import (App, RIO, logInfo)

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
