module Scrabble.WebSocket
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Monad          (forever)
import           Network.WebSockets     (ServerApp)

import           Scrabble.Types         (Communicate (..), Event (..),
                                         EventQueue, Gateway, Talk (..))

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception      as Exception
import qualified Network.WebSockets     as WS


--------------------------------------------------------------------------------
app :: EventQueue Gateway -> ServerApp
app gatewayQueue pendingConnection = do
  connection <- WS.acceptRequest pendingConnection
  WS.withPingThread connection 30 (pure ()) $ do
    authResponse <- WS.receiveData connection

    authQueue <- STM.atomically $ do
      queue <- STM.newTBQueue 256 -- todo
      emit gatewayQueue $ GatewayAuthenticate authResponse connection queue
      pure queue

    authResult <- STM.atomically $ STM.readTBQueue authQueue

    case authResult of
      Left err ->
        closeConnection connection err

      Right clientQueue ->
        Exception.finally onMessage onDisconnect
        where
          onMessage :: IO ()
          onMessage = forever $
            emitIO clientQueue . ClientMessageReceive
              =<< fromConnection connection

          onDisconnect :: IO ()
          onDisconnect =
            emitIO clientQueue ClientDisconnect
