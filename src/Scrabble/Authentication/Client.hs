-- | Operations for authenticating clients.
module Scrabble.Authentication.Client
  ( ClientAuth (..),
    ClientCache,
    ClientToken,
    ClientTokenError,
    Decoded,
    Encoded,
    HasClientAuth (..),
    Secret,
    cacheClient,
    clientTokenToLazyByteString,
    createCache,
    decodeClientToken,
    retrieveClientId,
    uncacheClient,
  )
where

import Data.Aeson (toJSON)
import qualified Data.Aeson as JSON
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import RIO hiding (timeout)
import qualified RIO.ByteString.Lazy as BL
import RIO.Time
import Scrabble.Authentication.Cache (Cache)
import qualified Scrabble.Authentication.Cache as Cache
import Scrabble.Authentication.Timeout (Timeout)
import qualified Scrabble.Authentication.Timeout as Timeout
import Scrabble.Authentication.Token (Decoded, Encoded, Secret, Token)
import qualified Scrabble.Authentication.Token as Token
import Scrabble.Client (Client, ID)

-- | The environment for authenticating clients.
data ClientAuth = ClientAuth
  { -- | A cache for tracking clients during the authentication process.
    authClientCache :: ClientCache,
    -- | The time clients have to complete authentication, in milliseconds.
    authExpireMilliseconds :: !Integer,
    -- | The HMAC secret for signing client tokens.
    authTokenSecret :: !Secret
  }

-- | A JSON Web Token provided to clients during authentication.
data ClientToken a where
  -- | A decoded client token.
  Decoded :: Token Decoded -> ClientToken Decoded
  -- | An encoded client token.
  Encoded :: Token Encoded -> ClientToken Encoded

-- | A cache for tracking clients during authentication.
data ClientCache
  = ClientCache (Cache Int (ID Client) (Timeout ()))

-- | A class for accessing the authentication environment.
class HasClientAuth env where
  -- | A lens for the authentication environment.
  clientAuthL :: Lens' env ClientAuth

-- | Possible errors encountered when decoding client tokens.
data ClientTokenError
  = -- | The text-encoding of the client token was malformed.
    ClientTokenMalformed
  | -- | The client ID was not present in the decoded token.
    ClientIdMissing

instance Display ClientTokenError where
  display = \case
    ClientTokenMalformed ->
      "Client token text was malformed"
    ClientIdMissing ->
      "Client did not specify an ID"

-- | Cache a new client. A timeout will be started to automatically uncache the
-- client if authentication is not completed within the expiration period.
cacheClient ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasClientAuth env
  ) =>
  -- | A token created for the new client.
  -- This token specifies the unique ID of the client.
  m (ClientToken Encoded)
cacheClient = do
  ClientAuth
    { authClientCache = ClientCache cache,
      authExpireMilliseconds = millis
    } <-
    view clientAuthL

  clientId <- Cache.insertWith cache $ \cid ->
    Timeout.create
      (fromInteger $ 1000 * millis)
      (void $ atomically $ Cache.remove cid cache)

  createClientToken clientId

-- | Uncache a client. The authentication timeout previously
-- set will be canceled if uncaching is successful.
uncacheClient ::
  ( MonadIO m,
    MonadReader env m,
    HasClientAuth env
  ) =>
  -- | The ID of the client to uncache.
  ID Client ->
  -- | True if the specified client existed and was
  -- successfully uncached, or false otherwise.
  m Bool
uncacheClient clientId =
  withClientCache $ \cache ->
    (atomically $ Cache.remove clientId cache) >>= \case
      Just timeout -> do
        Timeout.cancel timeout
        pure True
      Nothing ->
        pure False

-- | Perform an action with the client cache within the authentication environment.
withClientCache ::
  (MonadIO m, MonadReader env m, HasClientAuth env) =>
  (Cache Int (ID Client) (Timeout ()) -> m a) ->
  m a
withClientCache operation = do
  ClientAuth {authClientCache = ClientCache cache} <- view clientAuthL
  operation cache

-- | Create a client cache. This cache will atomically generate
-- unique IDs for inserted clients.
createCache :: STM (ClientCache)
createCache =
  ClientCache <$> Cache.create 0 (\count -> (fromString $ show count, count + 1))

-- | Retrieve the client ID specified in a client token.
retrieveClientId :: ClientToken Decoded -> Either ClientTokenError (ID Client)
retrieveClientId (Decoded token) =
  case Token.retrieveClaim "cid" token of
    Just clientId ->
      Right clientId
    Nothing ->
      Left ClientIdMissing

-- | Create a lazy bytestring from a text-encoded client token.
clientTokenToLazyByteString :: ClientToken Encoded -> BL.ByteString
clientTokenToLazyByteString (Encoded token) =
  Token.toLazyByteString token

-- | Decode a client token from text.
decodeClientToken ::
  ( MonadIO m,
    MonadReader env m,
    HasClientAuth env
  ) =>
  -- | The text to decode.
  Text ->
  -- | The decoded client token, or an error if decoding was not successful.
  m (Either ClientTokenError (ClientToken Decoded))
decodeClientToken tokenText = do
  ClientAuth {authTokenSecret = secret} <- view clientAuthL
  pure $ case Token.decodeFromText secret tokenText of
    Just token ->
      Right $ Decoded token
    Nothing ->
      Left ClientTokenMalformed

-- | Create an encoded client token.
createClientToken ::
  ( MonadIO m,
    MonadReader env m,
    HasClientAuth env
  ) =>
  -- | The ID of the client that will receive this token.
  ID Client ->
  -- | The encoded client token.
  m (ClientToken Encoded)
createClientToken clientId = do
  ClientAuth
    { authExpireMilliseconds = millis,
      authTokenSecret = secret
    } <-
    view clientAuthL

  expirationDate <- computeExpirationDate millis
  let token = Token.create (Just expirationDate) secret claims

  pure $ Encoded token
  where
    computeExpirationDate :: MonadIO m => Integer -> m NominalDiffTime
    computeExpirationDate milliseconds =
      (millisToDiffTime milliseconds +) . utcTimeToPOSIXSeconds <$> getCurrentTime

    millisToDiffTime :: Integer -> NominalDiffTime
    millisToDiffTime = fromInteger . (`div` 1000)

    claims :: [(Text, JSON.Value)]
    claims =
      [ ("cid", toJSON clientId)
      ]
