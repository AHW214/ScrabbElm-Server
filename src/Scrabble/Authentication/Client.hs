module Scrabble.Authentication.Client
  ( ClientAuth (..),
    ClientCache,
    ClientToken,
    HasClientAuth,
    Secret,
    cacheClient,
    createCache,
    isClientCached,
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
