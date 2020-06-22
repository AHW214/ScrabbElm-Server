module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent             (newMVar)
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.Wai.Middleware.Cors    (simpleCors)
import           System.Environment             (getArgs)
import           Text.Read                      (readMaybe)

import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.WebSockets             as WS

import qualified Scrabble.Request               as Request
import qualified Scrabble.Server                as Server
import qualified Scrabble.WebSocket             as WebSocket


--------------------------------------------------------------------------------
defaultPort :: Warp.Port
defaultPort = 3000


--------------------------------------------------------------------------------
readPort :: [ String ] -> Warp.Port
readPort =
  fromMaybe defaultPort . (=<<) readMaybe . listToMaybe


--------------------------------------------------------------------------------
main :: IO ()
main = do
  port <- readPort <$> getArgs
  putStrLn $ "Listening on port " ++ show port

  server <- newMVar Server.new

  let wsApp = WebSocket.app server
  let rqApp = simpleCors $ Request.app server

  Warp.run port $ websocketsOr WS.defaultConnectionOptions wsApp rqApp
