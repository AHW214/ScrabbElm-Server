module Scrabble.Authentication.Client
  ( ClientAuth (..),
    ClientCache,
    ClientToken,
    Decoded,
    Encoded,
    HasClientAuth,
    Secret,
    cacheClient,
    createCache,
    decodeClientToken,
    isClientCached,
    retrieveClientId,
    uncacheClient,
  )
where

import Data.Aeson (toJSON)
import qualified Data.Aeson as JSON
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import RIO
import RIO.Time
import Scrabble.Authentication.Cache
import qualified Scrabble.Authentication.Cache as Cache
import Scrabble.Authentication.Token
import qualified Scrabble.Authentication.Token as Token
import Scrabble.Client (Client, ID)

data ClientAuth = ClientAuth
  { authClientCache :: ClientCache,
    authExpireMilliseconds :: !Integer,
    authTokenSecret :: !Secret
  }

data ClientToken a where
  Decoded :: Token Decoded -> ClientToken Decoded
  Encoded :: Token Encoded -> ClientToken Encoded

data ClientCache = ClientCache (Cache Int (ID Client))

class HasClientAuth env where
  clientAuthL :: Lens' env ClientAuth

data ClientAuthError
  = MalformedToken
  | ClientIdMissing

instance Display ClientAuthError where
  display err =
    "Client authentication failed: " <> reason
    where
      reason =
        case err of
          MalformedToken ->
            "inbound token was malformed"
          ClientIdMissing ->
            "client did not specify an ID"

cacheClient :: (MonadIO m, MonadReader env m, HasClientAuth env) => m (ClientToken Encoded)
cacheClient =
  withClientCache $ createClientToken <=< atomically . Cache.add

isClientCached :: (MonadIO m, MonadReader env m, HasClientAuth env) => ID Client -> m Bool
isClientCached clientId =
  withClientCache $
    atomically . Cache.has clientId

uncacheClient :: (MonadIO m, MonadReader env m, HasClientAuth env) => ID Client -> m ()
uncacheClient clientId =
  withClientCache $
    atomically . Cache.remove clientId

withClientCache :: (MonadIO m, MonadReader env m, HasClientAuth env) => (Cache Int (ID Client) -> m a) -> m a
withClientCache operation = do
  ClientAuth {authClientCache = ClientCache cache} <- view clientAuthL
  operation cache

createCache :: STM (ClientCache)
createCache =
  ClientCache <$> Cache.create 0 (\count -> (fromString $ show count, count + 1))

retrieveClientId :: ClientToken Decoded -> Either ClientAuthError (ID Client)
retrieveClientId (Decoded token) =
  case retrieveClaim "cid" token of
    Just clientId ->
      Right clientId
    Nothing ->
      Left ClientIdMissing

decodeClientToken :: (MonadIO m, MonadReader env m, HasClientAuth env) => Text -> m (Either ClientAuthError (ClientToken Decoded))
decodeClientToken tokenText = do
  ClientAuth {authTokenSecret = secret} <- view clientAuthL
  pure $ case decodeFromText secret tokenText of
    Just token ->
      Right $ Decoded token
    Nothing ->
      Left MalformedToken

createClientToken :: (MonadIO m, MonadReader env m, HasClientAuth env) => ID Client -> m (ClientToken Encoded)
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
