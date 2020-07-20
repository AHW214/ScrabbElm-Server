module Scrabble.Gateway
  ( Secret
  , addTimeout
  , createClientId
  , createClientJWT
  , createSecret
  , deleteTimeout
  , new
  , removeTimeout
  , verifyClientJWT
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.Async (Async)
import           Control.Monad            ((<=<))
import           Data.Aeson               (FromJSON, Result (..), Value)
import           Data.Text                (Text)
import           Data.Time.Clock          (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX    (utcTimeToPOSIXSeconds)
import           Prelude                  hiding (exp)
import           System.Random            (RandomGen, StdGen)
import           Web.JWT                  (Algorithm (HS256),
                                           ClaimsMap (ClaimsMap, unClaimsMap),
                                           JOSEHeader (..), JWT, JWTClaimsSet (..),
                                           NumericDate, Signer, VerifiedJWT)

import           Scrabble.Random          (randomRSequence)
import           Scrabble.Types           (EventQueue, Gateway (..), Secret (..))

import qualified Data.Aeson               as JSON
import qualified Data.Bifunctor           as Bifunctor
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as Text
import qualified Data.Time.Clock          as Time
import qualified Web.JWT                  as JWT


--------------------------------------------------------------------------------
new :: StdGen -> Secret -> NominalDiffTime -> EventQueue Gateway -> Gateway
new stdGen secret timeoutLength queue = Gateway
  { gatewayAuthSecret    = secret
  , gatewayQueue         = queue
  , gatewayStdGen        = stdGen
  , gatewayTimeouts      = Map.empty
  , gatewayTimeoutLength = timeoutLength
  }


--------------------------------------------------------------------------------
createClientId :: Gateway -> ( Gateway, Text )
createClientId gateway@Gateway { gatewayStdGen } =
  ( gateway { gatewayStdGen = newStdGen }
  , clientId
  )
  where
    ( clientId, newStdGen ) =
      createId gatewayStdGen

    createId :: RandomGen g => g -> ( Text, g )
    createId =
      Bifunctor.first (("client-" <>) . Text.pack) . randomRSequence ( '0', '9' ) 10


--------------------------------------------------------------------------------
removeTimeout :: Text -> Gateway -> Maybe (Gateway, Async ())
removeTimeout clientId gateway =
  ( deleteTimeout clientId gateway
  , ) <$> getTimeout clientId gateway


--------------------------------------------------------------------------------
deleteTimeout :: Text -> Gateway -> Gateway
deleteTimeout clientId gateway@Gateway { gatewayTimeouts } = gateway
  { gatewayTimeouts = Map.delete clientId gatewayTimeouts
  }


--------------------------------------------------------------------------------
getTimeout :: Text -> Gateway -> Maybe (Async ())
getTimeout clientId Gateway { gatewayTimeouts } =
  Map.lookup clientId gatewayTimeouts


--------------------------------------------------------------------------------
addTimeout :: Text -> Async () -> Gateway -> Gateway
addTimeout clientId timeout gateway@Gateway { gatewayTimeouts } = gateway
  { gatewayTimeouts = Map.insert clientId timeout gatewayTimeouts
  }


--------------------------------------------------------------------------------
createSecret :: Text -> Secret
createSecret = Secret . JWT.hmacSecret


--------------------------------------------------------------------------------
createClientJWT :: Text -> Gateway -> IO Text
createClientJWT
  clientId
  Gateway
    { gatewayAuthSecret = Secret signer
    , gatewayTimeoutLength
    }
  = withTime <$> Time.getCurrentTime
    where
      withTime :: UTCTime -> Text
      withTime time =
        createJWT (expirationDate time) signer clientClaims

      expirationDate :: UTCTime -> Maybe NumericDate
      expirationDate =
        JWT.numericDate . (gatewayTimeoutLength +) . utcTimeToPOSIXSeconds

      clientClaims :: [ ( Text, JSON.Value ) ]
      clientClaims =
        [ ( "cid", JSON.String clientId )
        ]


--------------------------------------------------------------------------------
createJWT :: Maybe NumericDate -> Signer -> [ ( Text, JSON.Value ) ] -> Text
createJWT expirationDate signer unregClaims =
  JWT.encodeSigned signer header claims
  where
    header :: JOSEHeader
    header = mempty
      { typ = Just "JWT"
      , alg = Just HS256
      }

    claims :: JWTClaimsSet
    claims = mempty
      { iss = JWT.stringOrURI "ScrabbElm-Server"
      , sub = JWT.stringOrURI "ScrabbElm-Client"
      , exp = expirationDate
      , unregisteredClaims =
          ClaimsMap $ Map.fromList unregClaims
      }


--------------------------------------------------------------------------------
verifyClientJWT :: Gateway -> Text -> Maybe Text
verifyClientJWT Gateway { gatewayAuthSecret = Secret signer } =
  getClientClaims <=< JWT.decodeAndVerifySignature signer


--------------------------------------------------------------------------------
getClientClaims :: JWT VerifiedJWT -> Maybe Text
getClientClaims jwt =
  claimField "cid"
  where
    claimField :: FromJSON a => Text -> Maybe a
    claimField fieldName =
      maybeFromJSON =<< Map.lookup fieldName (unClaimsMap unregClaims)

    unregClaims :: ClaimsMap
    unregClaims =
      unregisteredClaims $ JWT.claims jwt

    maybeFromJSON :: FromJSON a => Value -> Maybe a
    maybeFromJSON value =
      case JSON.fromJSON value of
        Success x -> Just x
        _         -> Nothing
