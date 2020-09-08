module Scrabble.WebSockets
  ( app,
  )
where

import Network.WebSockets (Connection, PendingConnection)
import qualified Network.WebSockets as WS
import RIO
import Scrabble.App (App)

app :: PendingConnection -> RIO App ()
app pendingConnection = do
  connection <- acceptRequest pendingConnection
  logInfo "Incoming pending connection!"

  withClientThread connection $ do
    message <- receiveMessage connection
    logInfo $ "Connection said '" <> display message <> "'"

    case message of
      "meme" -> do
        logInfo "Correct secret message - connection accepted!"
        finally onMessage onDisconnect
        where
          onMessage :: RIO App ()
          onMessage = forever $ do
            msg <- receiveMessage connection
            logInfo $ "Connection said '" <> display msg <> "'"
            sendMessage connection $ "You said: " <> msg

          onDisconnect :: RIO App ()
          onDisconnect =
            logInfo "Goodbye connection"
      _ -> do
        logInfo $ "Incorrect secret message - disconnecting..."
        closeConnection connection "Goodbye."

withClientThread :: Connection -> RIO App () -> RIO App ()
withClientThread connection action = withRunInIO $ \runInIO ->
  WS.withPingThread connection pingInterval (pure ()) $ runInIO action

pingInterval :: Int
pingInterval = 30

sendMessage :: Connection -> Text -> RIO App ()
sendMessage connection = liftIO . WS.sendTextData connection

receiveMessage :: Connection -> RIO App Text
receiveMessage = liftIO . WS.receiveData

closeConnection :: Connection -> Text -> RIO App ()
closeConnection connection = liftIO . WS.sendClose connection

acceptRequest :: PendingConnection -> RIO App Connection
acceptRequest = liftIO . WS.acceptRequest
