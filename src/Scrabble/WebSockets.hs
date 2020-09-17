module Scrabble.WebSockets
  ( app,
  )
where

import Control.Monad.Except
import qualified Network.HTTP.Types as HTTP
import Network.WebSockets
  ( Connection,
    ConnectionException,
    PendingConnection,
    RequestHead (..),
    pendingRequest,
  )
import qualified Network.WebSockets as WS
import RIO
import Scrabble.Authentication.Client
import Scrabble.Client (Client)
import Scrabble.Common (ID)

data WebSocketAuthError
  = ClientAuthError ClientAuthError
  | RequestMissingToken
  | RequestTokenDecodeError UnicodeException
  | UnknownClientId (ID Client)

instance Display WebSocketAuthError where
  display = \case
    ClientAuthError err ->
      "Client authentication error: " <> display err
    RequestMissingToken ->
      "Token missing from websocket request"
    RequestTokenDecodeError err ->
      "Error decoding request token: " <> displayShow err
    UnknownClientId clientId ->
      "Unknown client ID '" <> display clientId <> "' (probably timed out)"

app :: (HasClientAuth env, HasLogFunc env) => PendingConnection -> RIO env ()
app pendingConnection = do
  logInfo "Incoming pending connection!"

  runExceptT (authenticate pendingConnection) >>= \case
    Right clientId -> do
      connection <- acceptRequest pendingConnection
      logInfo $ "Connection accepted as " <> display clientId <> "!"
      serveClient clientId connection
    Left err -> do
      logWarn $ "Connection failed to authenticate: " <> display err
      rejectRequest pendingConnection $ encodeUtf8 $ textDisplay err

serveClient :: forall env. HasLogFunc env => ID Client -> Connection -> RIO env ()
serveClient clientId connection =
  withClientThread connection $ onMessage `finally` onDisconnect
  where
    onMessage :: RIO env ()
    onMessage =
      try (receiveMessage connection) >>= \case
        Right msg -> do
          logInfo $ display clientId <> " said '" <> display msg <> "'"
          sendMessage connection $ "You said: " <> msg
          onMessage
        Left (excp :: ConnectionException) ->
          logInfo $ display clientId <> " disconnected: " <> displayShow excp

    onDisconnect :: RIO env ()
    onDisconnect =
      logInfo "Goodbye connection"

withClientThread :: Connection -> RIO a () -> RIO a ()
withClientThread connection action = withRunInIO $ \runInIO ->
  WS.withPingThread connection pingInterval (pure ()) $ runInIO action

authenticate ::
  forall env.
  HasClientAuth env =>
  PendingConnection ->
  ExceptT WebSocketAuthError (RIO env) (ID Client)
authenticate =
  verifyClientId
    <=< liftEither
      . first ClientAuthError
      . retrieveClientId
    <=< withExceptT ClientAuthError
      . ExceptT
      . decodeClientToken
    <=< liftEither
      . tokenTextFromRequest
      . pendingRequest
  where
    verifyClientId :: ID Client -> ExceptT WebSocketAuthError (RIO env) (ID Client)
    verifyClientId clientId = do
      isValidId <- uncacheClient clientId
      if isValidId
        then pure clientId
        else throwError $ UnknownClientId clientId

tokenTextFromRequest :: RequestHead -> Either WebSocketAuthError Text
tokenTextFromRequest RequestHead {requestPath} =
  case HTTP.decodePath requestPath of
    (_, [("token", Just token)]) ->
      first RequestTokenDecodeError $ decodeUtf8' token
    _ ->
      Left RequestMissingToken

pingInterval :: Int
pingInterval = 30

sendMessage :: Connection -> Text -> RIO a ()
sendMessage connection = liftIO . WS.sendTextData connection

receiveMessage :: Connection -> RIO a Text
receiveMessage = liftIO . WS.receiveData

rejectRequest :: PendingConnection -> ByteString -> RIO a ()
rejectRequest pendingConnection =
  liftIO . WS.rejectRequest pendingConnection

acceptRequest :: PendingConnection -> RIO a Connection
acceptRequest = liftIO . WS.acceptRequest
