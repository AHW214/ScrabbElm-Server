module Scrabble.WebSockets
  ( app,
  )
where

import Network.WebSockets (ServerApp)
import qualified Network.WebSockets as WS
import RIO

app :: ServerApp
app pendingConnection = do
  connection <- WS.acceptRequest pendingConnection
  WS.withPingThread connection pingInterval (pure ()) $ do
    message <- WS.receiveData connection :: IO Text

    case message of
      "meme" ->
        finally onMessage onDisconnect
        where
          onMessage :: IO ()
          onMessage = forever $ do
            msg <- WS.receiveData connection :: IO Text
            WS.sendTextData connection msg

          onDisconnect :: IO ()
          onDisconnect = pure ()
      _ ->
        WS.sendClose connection ("Goodbye." :: Text)

pingInterval :: Int
pingInterval = 30
