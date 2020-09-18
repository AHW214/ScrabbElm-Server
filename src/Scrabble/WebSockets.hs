-- | Application for WebSocket communications.
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

-- | Type alias for WebSocket authentication operations.
type WebSocketAuth env a b =
  a -> ExceptT WebSocketAuthError (RIO env) b

-- | Possible errors encountered during WebSocket authentication.
data WebSocketAuthError
  = -- | The WebSocket request was invalid.
    InvalidRequest RequestError
  | -- | The returned client token was invalid.
    InvalidToken ClientTokenError
  | -- | The client ID specified was not recognized.
    UnknownClient (ID Client)

instance Display WebSocketAuthError where
  display = \case
    InvalidRequest err ->
      "WebSocket request error: "
        <> display err
    InvalidToken err ->
      "Client token error: "
        <> display err
    UnknownClient clientId ->
      "Unknown client ID '"
        <> display clientId
        <> "' (probably timed out)"

-- | Possible errors concerning WebSocket requests.
data RequestError
  = -- | The WebSocket request did not provide a client token.
    RequestMissingToken
  | -- | The WebSocket request did not properly encode the provided client token.
    RequestTokenBadUnicode UnicodeException

instance Display RequestError where
  display = \case
    RequestMissingToken ->
      "No token specified"
    RequestTokenBadUnicode excp ->
      "Could not read token text: "
        <> displayShow excp

-- | The WebSocket application.
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

-- | Serve a new client.
serveClient :: forall env. HasLogFunc env => ID Client -> Connection -> RIO env ()
serveClient clientId connection =
  withClientThread connection $ onMessage `finally` onDisconnect
  where
    -- Handle messages from the client.
    onMessage :: RIO env ()
    onMessage =
      try (receiveMessage connection) >>= \case
        Right msg -> do
          logInfo $ display clientId <> " said '" <> display msg <> "'"
          sendMessage connection $ "You said: " <> msg
          onMessage
        Left (excp :: ConnectionException) ->
          logInfo $ display clientId <> " disconnected: " <> displayShow excp

    -- Handle disconnection.
    onDisconnect :: RIO env ()
    onDisconnect =
      logInfo "Goodbye connection"

-- | Perform an operation in a new thread for a client.
-- The spawned thread will ping the client regularly to
-- maintain the WebSocket connection.
withClientThread :: Connection -> RIO a () -> RIO a ()
withClientThread connection action = withRunInIO $ \runInIO ->
  WS.withPingThread connection pingInterval (pure ()) $ runInIO action

-- | Authenticate a new WebSocket client.
authenticate ::
  forall env.
  HasClientAuth env =>
  WebSocketAuth env PendingConnection (ID Client)
authenticate =
  verifyId
    <=< retrieveId
    <=< decodeToken
    <=< readToken
  where
    -- Verify that the client ID exists in the client cache.
    verifyId :: WebSocketAuth env (ID Client) (ID Client)
    verifyId clientId = do
      isValidId <- uncacheClient clientId
      if isValidId
        then pure clientId
        else throwError $ UnknownClient clientId

    -- Retrieve the ID specified by the client token.
    retrieveId :: WebSocketAuth env (ClientToken Decoded) (ID Client)
    retrieveId =
      withExceptT InvalidToken
        . liftEither
        . retrieveClientId

    -- Decode the client token from its text encoding.
    decodeToken :: WebSocketAuth env Text (ClientToken Decoded)
    decodeToken =
      withExceptT InvalidToken
        . ExceptT
        . decodeClientToken

    -- Read the text-encoded client token from the WebSocket request.
    readToken :: WebSocketAuth env PendingConnection Text
    readToken =
      withExceptT InvalidRequest
        . liftEither
        . tokenTextFromRequest
        . pendingRequest

-- | Try to read a text-encoded client token from a WebSocket request.
tokenTextFromRequest :: RequestHead -> Either RequestError Text
tokenTextFromRequest RequestHead {requestPath} =
  case HTTP.decodePath requestPath of
    (_, [("token", Just token)]) ->
      first RequestTokenBadUnicode $ decodeUtf8' token
    _ ->
      Left RequestMissingToken

-- | The interval on which to ping WebSocket clients, in milliseconds.
pingInterval :: Int
pingInterval = 30

-- | Send a textual message to a WebSocket connection.
sendMessage :: Connection -> Text -> RIO a ()
sendMessage connection = liftIO . WS.sendTextData connection

-- | Receive a textual message from a WebSocket connection.
receiveMessage :: Connection -> RIO a Text
receiveMessage = liftIO . WS.receiveData

-- | Reject a WebSocket request with the provided reason.
rejectRequest :: PendingConnection -> ByteString -> RIO a ()
rejectRequest pendingConnection =
  liftIO . WS.rejectRequest pendingConnection

-- | Accept a WebSocket request.
acceptRequest :: PendingConnection -> RIO a Connection
acceptRequest = liftIO . WS.acceptRequest
