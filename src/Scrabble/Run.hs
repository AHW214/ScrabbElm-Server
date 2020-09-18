-- | Run the program.
module Scrabble.Run
  ( run,
  )
where

import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.WebSockets as WS
import RIO
import Scrabble.App (App)
import qualified Scrabble.Request as Request
import qualified Scrabble.WebSockets as WebSockets

-- | Run the web server on the provided port.
run :: Port -> RIO App ()
run port = do
  logInfo $ "Listening on port " <> display port

  withRunInIO $ \runInIO ->
    Warp.run port $
      websocketsOr
        WS.defaultConnectionOptions
        (runInIO . WebSockets.app)
        (simpleCors $ \req -> runInIO . Request.app req)
