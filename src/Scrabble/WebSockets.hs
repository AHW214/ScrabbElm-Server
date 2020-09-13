module Scrabble.WebSockets
  ( app,
  )
where

import Network.WebSockets (Connection, PendingConnection)
import qualified Network.WebSockets as WS
import RIO
import Scrabble.App
import qualified Scrabble.Authentication as Auth
import Scrabble.Client (Client)
import Scrabble.Common (ID)

app :: (HasLogFunc env, HasPendingClients env) => PendingConnection -> RIO env ()
app pendingConnection = do
  connection <- acceptRequest pendingConnection
  logInfo "Incoming pending connection!"

  withClientThread connection $ do
    authenticated <- authenticate connection

    case authenticated of
      Right clientId -> do
        logInfo $ "Connection accepted as " <> display clientId <> "!"
        onMessage `finally` onDisconnect
        where
          onMessage :: HasLogFunc env => RIO env ()
          onMessage = forever $ do
            msg <- receiveMessage connection
            logInfo $ "Connection said '" <> display msg <> "'"
            sendMessage connection $ "You said: " <> msg

          onDisconnect :: HasLogFunc env => RIO env ()
          onDisconnect =
            logInfo "Goodbye connection"
      Left badAuth -> do
        logWarn $ "Connection failed to authenticate: " <> badAuth
        closeConnection connection "Goodbye."

authenticate :: HasPendingClients env => Connection -> RIO env (Either Utf8Builder (ID Client))
authenticate connection = do
  auth <- receiveMessage connection
  case Auth.clientIdFromJWT "secret" auth of
    Just clientId -> do
      isPendingClient <- removePendingClient clientId

      pure $
        if isPendingClient
          then Right clientId
          else Left "wasnt pending client"
    _ ->
      pure $ Left "bad auth"

withClientThread :: Connection -> RIO a () -> RIO a ()
withClientThread connection action = withRunInIO $ \runInIO ->
  WS.withPingThread connection pingInterval (pure ()) $ runInIO action

pingInterval :: Int
pingInterval = 30

sendMessage :: Connection -> Text -> RIO a ()
sendMessage connection = liftIO . WS.sendTextData connection

receiveMessage :: Connection -> RIO a Text
receiveMessage = liftIO . WS.receiveData

closeConnection :: Connection -> Text -> RIO a ()
closeConnection connection = liftIO . WS.sendClose connection

acceptRequest :: PendingConnection -> RIO a Connection
acceptRequest = liftIO . WS.acceptRequest
