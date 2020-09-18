-- | Types and instances for the application environment.
module Scrabble.App
  ( App (..),
  )
where

import RIO
import RIO.Process (HasProcessContext (..), ProcessContext)
import Scrabble.Authentication.Client

-- | The application environment.
data App = App
  { -- | The environment for authenticating new clients.
    appClientAuth :: ClientAuth,
    -- | A function for logging diagnostic information.
    appLogFunc :: !LogFunc,
    -- | The environment for reading process information [TODO: not needed?].
    appProcessContext :: !ProcessContext
  }

instance HasClientAuth App where
  clientAuthL = lens appClientAuth (\x y -> x {appClientAuth = y})

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
