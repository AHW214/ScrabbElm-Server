-- Based on https://github.com/jaspervdj/websockets/blob/master/example/server.lhs

{-# LANGUAGE OverloadedStrings #-}

module Server where
  ( Client
  , ServerState
  , Handler
  , numClients
  , send
  , broadcast
  , start
  )

import Data.Monoid ((<>))
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

type Client
  = ( Text, WS.Connection )

type ServerState
  = [ Client ]

initServerState :: ServerState
initServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists (name, _) = any ((== name) . fst)

addClient :: Client -> ServerState -> ServerState
addClient = (:)

removeClient :: Client -> ServerState -> ServerState
removeClient (name, _) = filter ((/= name) . fst)

send :: Text -> Client -> IO ()
send message (name, conn) = do
  T.putStrLn ("To client '" <> name <> "': " <> "\"" <> message <> "\"")
  WS.sendTextData conn message

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn ("Broadcast: " <> "\"" <> message <> "\"")
  forM_ clients (flip WS.sendTextData message . snd)

start :: String -> Int -> Handler -> IO ()
start host port handler = do
  state <- newMVar initServerState
  WS.runServer host port $ application handler state

application :: Handler -> MVar ServerState -> WS.ServerApp
application handler state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  name <- WS.receiveData conn
  clients <- readMVar state

  case name of
    _ | clientExists client clients ->
          send ("Username taken: '" <> name <> "'") client
      | otherwise ->
          flip finally disconnect $ do
            modifyMVar_ state $ \s -> do
              let s' = addClient client s
              send ("Connected. Users: " <> T.intercalate ", " (map fst s)) client
              broadcast (fst client <> " joined") s'
              return s'
            handleMessage handler client state
      where
        client =
          (name, conn)

        disconnect = do
          s <- modifyMVar state $ \s ->
            let s' = removeClient client s in return (s', s')
          broadcast (fst client <> " disconnected") s

type Handler
  = Text -> Client -> ServerState -> IO ()

handleMessage :: Handler -> Client -> MVar ServerState -> IO ()
handleMessage handler client state = forever $ do
  msg <- WS.receiveData (snd client)
  clients <- readMVar state
  handler msg client clients