module Main where

--------------------------------------------------------------------------------

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Data.Maybe (fromMaybe, listToMaybe)
import Network.Wai.Handler.Warp (Port)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.WebSockets as WS
import Scrabble.Config (Config (..))
import qualified Scrabble.Config as Config
import qualified Scrabble.Gateway as Gateway
import Scrabble.Handler
  ( gatewayHandler,
    lobbyHandler,
    processQueue,
  )
import qualified Scrabble.Lobby as Lobby
import Scrabble.Log (LogLevel (..), Logger (..))
import qualified Scrabble.Log as Log
import qualified Scrabble.Request as Request
import Scrabble.Types (Context (..), Talk (..))
import qualified Scrabble.WebSocket as WebSocket
import System.Environment (getArgs)
import qualified System.Random as Random
import Text.Read (readMaybe)
import TextShow (showt)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  config@Config
    { configLogLevel,
      configPort
    } <-
    Config.load

  loggerQueue <- STM.newTBQueueIO 256 -- todo
  gatewayQueue <- newQueueIO
  lobbyQueue <- newQueueIO

  let context =
        Context
          { contextLobbyQueue = lobbyQueue,
            contextLoggerQueue = loggerQueue,
            contextLogLevel = configLogLevel
          }

  gatewayStdGen <- Random.getStdGen

  let gateway =
        Gateway.new
          config
          gatewayStdGen
          gatewayQueue

  let lobby = Lobby.new lobbyQueue

  logOnThread LogInfo "Starting logger..."
  Async.async $ Log.runLogger context

  logOnThread LogInfo "Starting gateway..."
  Async.async $ processQueue (gatewayHandler context) gatewayQueue gateway

  logOnThread LogInfo "Starting lobby..."
  Async.async $ processQueue (lobbyHandler context) lobbyQueue lobby

  let rqApp = simpleCors $ Request.app gatewayQueue
  let wsApp = WebSocket.app gatewayQueue

  port <- fromMaybe configPort <$> readCustomPort

  logOnThread LogInfo $ "Listening on port " <> showt port
  Warp.run port $ websocketsOr WS.defaultConnectionOptions wsApp rqApp
  where
    readCustomPort :: IO (Maybe Port)
    readCustomPort = readPort <$> getArgs

--------------------------------------------------------------------------------
readPort :: [String] -> Maybe Port
readPort = (readMaybe =<<) . listToMaybe
