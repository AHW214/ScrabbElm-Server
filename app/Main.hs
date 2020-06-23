module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent             (newMVar)
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Network.Wai.Handler.Warp       (Port)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.Wai.Middleware.Cors    (simpleCors)
import           System.Directory               (doesFileExist)
import           System.Environment             (getArgs)
import           System.Exit                    (exitFailure)
import           Text.Read                      (readMaybe)
import           TextShow                       (showt)

import           Scrabble.Config                (Config (..))

import qualified Data.Text                      as T
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
  configExists <- doesFileExist Config.path

  initConfig@Config { configPort } <-
    if configExists then do
      res <- Config.load Config.path
      case res of
        Left err -> do
          T.putStrLn $ "Error parsing config: " <> T.pack err
          exitFailure

        Right config ->
          pure config
    else do
      T.putStrLn $ "Warning: using placeholder configuration"
      pure Config.placeholder

  customPort <- readPort <$> getArgs

  let config = initConfig { configPort = customPort <|> configPort }

  server <- newMVar $ Server.new config

  let wsApp = WebSocket.app server
  let rqApp = simpleCors $ Request.app server

  T.putStrLn $ "Listening on port " <> showt (configPort config)

  Warp.run config.port $ websocketsOr WS.defaultConnectionOptions wsApp rqApp


--------------------------------------------------------------------------------
readPort :: [ String ] -> Maybe Port
readPort = (readMaybe =<<) . listToMaybe
