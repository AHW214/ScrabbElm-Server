-- Based on https://github.com/jaspervdj/websockets/blob/master/example/server.lhs

{-# LANGUAGE OverloadedStrings #-}

module WebSockets
  ( Client
  , ServerState
  , Handler
  , numClients
  , send
  , broadcast
  , initApp
  , opts
  , app
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.WebSockets.Connection (ConnectionOptions, defaultConnectionOptions)
import qualified Network.WebSockets as WS
import Tickets (Ticket)

type Client
  = (Text, WS.Connection)

type ServerState
  = Map.Map Ticket Client

initServerState :: ServerState
initServerState = Map.empty

numClients :: ServerState -> Int
numClients = Map.size

clientExists :: Ticket -> ServerState -> Bool
clientExists = Map.member

addClient :: Ticket -> Client -> ServerState -> ServerState
addClient = Map.insert

removeClient :: Ticket -> ServerState -> ServerState
removeClient = Map.delete

close :: Text -> Client -> IO ()
close message (name, conn) = do
  T.putStrLn ("Disconnecting client '" <> name <> "'");
  WS.sendClose conn message

send :: Text -> Client -> IO ()
send message (name, conn) = do
  T.putStrLn ("To client '" <> name <> "': " <> "\"" <> message <> "\"")
  WS.sendTextData conn message

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn ("Broadcast: " <> "\"" <> message <> "\"")
  forM_ clients (flip WS.sendTextData message . snd)

opts :: ConnectionOptions
opts = defaultConnectionOptions

initApp :: Handler -> MVar [ Ticket ] -> IO WS.ServerApp
initApp handler tickets = do
  state <- newMVar initServerState
  return $ app handler tickets state

app :: Handler -> MVar [ Ticket ] -> MVar ServerState -> WS.ServerApp
app handler tickets state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  ticket <- WS.receiveData conn
  clients <- readMVar state
  ts <- readMVar tickets

  case ticket of
    _ | not $ elem ticket ts ->
          close "Invalid authentication ticket..." client
      | otherwise ->
          flip finally disconnect $ do
            modifyMVar_ tickets $ pure . filter ((/=) ticket)
            modifyMVar_ state $ \s -> do
              let s' = addClient ticket client s
              send ("Connected. Users: " <> T.intercalate ", " (map fst $ Map.elems s)) client
              broadcast (fst client <> " joined") s'
              return s'
            handleMessages handler client state
      where
        client =
          ("NAME", conn)

        disconnect = do
          s <- modifyMVar state $ \s ->
            let s' = removeClient ticket s in return (s', s')
          broadcast (fst client <> " disconnected") s

type Handler
  = Text -> Client -> ServerState -> IO ()

handleMessages :: Handler -> Client -> MVar ServerState -> IO ()
handleMessages handler client state = forever $ do
  msg <- WS.receiveData (snd client)
  clients <- readMVar state
  handler msg client clients