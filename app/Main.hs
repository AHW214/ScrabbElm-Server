module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent             (newMVar)
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
import           Scrabble.Log                   (Log (..), LogLevel (..))

import qualified Control.Exception              as Exception
import qualified Data.ByteString.Char8          as BSS
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.WebSockets             as WS

import qualified Scrabble.Config                as Config
import qualified Scrabble.Request               as Request
import qualified Scrabble.Server                as Server
import qualified Scrabble.WebSocket             as WebSocket


--------------------------------------------------------------------------------
main :: IO ()
main = do
  config@Config { configPort } <- loadConfig
  port <- fromMaybe configPort <$> readCustomPort

  let server = Server.new config
  mServer <- newMVar server

  let wsApp = WebSocket.app mServer
  let rqApp = simpleCors $ Request.app mServer

  logInfo server $ "Listening on port " <> showt port

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
              logAs LogError ("Failed to decode config (" <> errMsg <> ")")
              >> exitFailure

        _ ->
          logAs LogWarning "Using placeholder config"
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
