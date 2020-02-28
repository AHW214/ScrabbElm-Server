{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (newMVar)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import qualified Request
import qualified Server
import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified WebSocket

defaultPort :: Warp.Port
defaultPort = 3000

readPort :: [ String ] -> Warp.Port
readPort =
  fromMaybe defaultPort . (=<<) readMaybe . listToMaybe

main :: IO ()
main = do
  port <- readPort <$> getArgs
  putStrLn $ "Listening on port " ++ show port

  server <- newMVar Server.new

  let wsApp = WebSocket.app server
  let rqApp = simpleCors $ Request.app server

  Warp.run port $ websocketsOr WS.defaultConnectionOptions wsApp rqApp
