-- Based on https://github.com/jaspervdj/websockets/blob/master/example/server.lhs

{-# LANGUAGE OverloadedStrings #-}

module WebSockets
  ( Client
  , ServerState
  , Handlers(..)
  , numClients
  , send
  , broadcast
  , close
  , initApp
  , opts
  , app
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
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
  = [ Client ]

type OnConnect
  = Client -> ServerState -> IO ()

type OnDisconnect
  = Client -> ServerState -> IO ()

type OnMessage
  = Text -> Client -> ServerState -> IO ()

data Handlers
  = Handlers
      { onConnect :: OnConnect
      , onMessage :: OnMessage
      , onDisconnect :: OnDisconnect
      }

maxNumClients :: Int
maxNumClients = 2

initServerState :: ServerState
initServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists (name, _) = any ((==) name . fst)

addClient :: Client -> ServerState -> ServerState
addClient = (:)

removeClient :: Client -> ServerState -> ServerState
removeClient (name, _) = filter ((/=) name . fst)

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

initApp :: Handlers -> MVar [ Ticket ] -> IO WS.ServerApp
initApp handlers tickets = do
  state <- newMVar initServerState
  return $ app handlers tickets state

app :: Handlers -> MVar [ Ticket ] -> MVar ServerState -> WS.ServerApp
app ( Handlers onConnect onMessage onDisconnect ) tickets state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  ticket <- WS.receiveData conn
  clients <- readMVar state
  ts <- readMVar tickets

  case ticket of
    _ | not $ elem ticket ts ->
          close "Invalid authentication ticket..." client
      | numClients clients == maxNumClients ->
          close ("Max number of clients (" <> (T.pack $ show maxNumClients) <> ") already connected") client
      | otherwise ->
          flip finally disconnect $ do
            T.putStrLn ("Client '" <> fst client <> "' joined")
            modifyMVar_ tickets $ pure . filter ((/=) ticket)

            newClients <- modifyMVar state $ \s ->
              let s' = addClient client s in return (s', s')

            onConnect client newClients
            handleMessages onMessage client state
      where
        client =
          (toStrict $ decodeUtf8 ticket, conn)

        disconnect = do
          newState <- modifyMVar state $ \s ->
            let s' = removeClient client s in return (s', s')
          onDisconnect client newState
          T.putStrLn ("Client '" <> fst client <> "' disconnected")

handleMessages :: OnMessage -> Client -> MVar ServerState -> IO ()
handleMessages handler client state = forever $ do
  msg <- WS.receiveData (snd client)
  clients <- readMVar state
  handler msg client clients