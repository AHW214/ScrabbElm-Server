module Main where


--------------------------------------------------------------------------------
import           Control.Monad                  (guard)
import           Data.ByteString                (ByteString)
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Network.Wai.Handler.Warp       (Port)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.Wai.Middleware.Cors    (simpleCors)
import           System.Environment             (getArgs)
import           System.Exit                    (exitFailure)
import           System.IO.Error                (isDoesNotExistError)
import           Text.Read                      (readMaybe)
import           TextShow                       (showt)

import           Scrabble.Config                (Config (..))
import           Scrabble.Handler               (gatewayHandler, lobbyHandler,
                                                 processQueue)
import           Scrabble.Log                   (Logger (..), LogLevel (..))
import           Scrabble.Types                 (Context (..), Talk (..))

import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.STM         as STM
import qualified Control.Exception              as Exception
import qualified Data.ByteString.Char8          as BSS
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.WebSockets             as WS
import qualified System.Random                  as Random

import qualified Scrabble.Config                as Config
import qualified Scrabble.Gateway               as Gateway
import qualified Scrabble.Lobby                 as Lobby
import qualified Scrabble.Log                   as Log
import qualified Scrabble.Request               as Request
import qualified Scrabble.WebSocket             as WebSocket


--------------------------------------------------------------------------------
main :: IO ()
main = do
  config@Config
    { configLogLevel
    , configPort
    } <- loadConfig

  loggerQueue <- STM.newTBQueueIO 256 -- todo
  gatewayQueue <- newQueueIO
  lobbyQueue <- newQueueIO

  let context = Context
        { contextLobbyQueue  = lobbyQueue
        , contextLoggerQueue = loggerQueue
        , contextLogLevel    = configLogLevel
        }

  gatewayStdGen <- Random.getStdGen

  let gateway = Gateway.new
        config
        gatewayStdGen
        gatewayQueue

  let lobby = Lobby.new lobbyQueue

  Async.async $ Log.runLogger context
  Async.async $ processQueue (gatewayHandler context) gatewayQueue gateway
  Async.async $ processQueue (lobbyHandler context) lobbyQueue lobby

  let rqApp = simpleCors $ Request.app gatewayQueue
  let wsApp = WebSocket.app gatewayQueue

  port <- fromMaybe configPort <$> readCustomPort

  logInfo context $ "Listening on port " <> showt port

  Warp.run port $ websocketsOr WS.defaultConnectionOptions wsApp rqApp
  where
    loadConfig :: IO Config
    loadConfig =
      readFileSafe Config.path >>= \case
        Just configJson ->
          case Config.decode configJson of
            Right config ->
              pure config

            Left errMsg ->
              logOnThread LogError ("Failed to decode config (" <> errMsg <> ")")
              >> exitFailure

        _ ->
          logOnThread LogWarning "Using placeholder config"
          >> pure Config.placeholder

    readCustomPort :: IO (Maybe Port)
    readCustomPort = readPort <$> getArgs


--------------------------------------------------------------------------------
readPort :: [ String ] -> Maybe Port
readPort = (readMaybe =<<) . listToMaybe


--------------------------------------------------------------------------------
readFileSafe :: FilePath -> IO (Maybe ByteString)
readFileSafe filePath =
  Exception.catchJust whenDoesNotExist readFileMaybe handleNothing
  where
    whenDoesNotExist :: IOError -> Maybe ()
    whenDoesNotExist =
      guard . isDoesNotExistError

    readFileMaybe :: IO (Maybe ByteString)
    readFileMaybe =
      Just <$> BSS.readFile filePath

    handleNothing :: () -> IO (Maybe ByteString)
    handleNothing =
      pure . const Nothing
