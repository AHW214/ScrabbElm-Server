module Main where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<|>))
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

import qualified Control.Exception              as Exception
import qualified Data.ByteString.Char8          as BSS
import qualified Data.Foldable                  as Fold
import qualified Data.Text.IO                   as T
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
  customPort <- readCustomPort

  let port = choosePort defaultPort [ customPort, configPort ]

  server <- newMVar $ Server.new config

  let wsApp = WebSocket.app server
  let rqApp = simpleCors $ Request.app server

  T.putStrLn $ "Listening on port " <> showt port

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
              T.putStrLn ("Error: failed to decode config (" <> errMsg <> ")")
              >> exitFailure

        _ ->
          T.putStrLn "Warning: using placeholder config"
          >> pure Config.placeholder

    readCustomPort :: IO (Maybe Port)
    readCustomPort = readPort <$> getArgs

    choosePort :: Foldable t => Port -> t (Maybe Port) -> Port
    choosePort defPort =
      fromMaybe defPort . Fold.foldl' (<|>) Nothing


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


--------------------------------------------------------------------------------
defaultPort :: Port
defaultPort = 3000
