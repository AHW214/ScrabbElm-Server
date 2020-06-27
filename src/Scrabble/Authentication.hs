module Scrabble.Authentication
  ( Secret
  , createClientJwt
  , createSecret
  , createTicket
  , verifyClientJwt
  ) where


--------------------------------------------------------------------------------
import           Control.Monad         ((<=<))
import           Data.Aeson            (FromJSON (parseJSON), Result (..),
                                        Value)
import           Data.Text             (Text)
import           Data.Time.Clock       (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Prelude               hiding (exp)
import           System.Random         (getStdRandom)
import           Web.JWT               (Algorithm (HS256),
                                        ClaimsMap (ClaimsMap, unClaimsMap),
                                        JOSEHeader (..), JWT, JWTClaimsSet (..),
                                        NumericDate, Signer, VerifiedJWT)

import           Scrabble.Random       (randomRSequence)

import qualified Data.Aeson            as JSON
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as Text
import qualified Data.Time.Clock       as Time
import qualified Web.JWT               as JWT


--------------------------------------------------------------------------------
newtype Secret = Secret Signer


--------------------------------------------------------------------------------
instance FromJSON Secret where
  parseJSON = fmap createSecret . parseJSON


--------------------------------------------------------------------------------
createSecret :: Text -> Secret
createSecret = Secret . JWT.hmacSecret


--------------------------------------------------------------------------------
createTicket :: Int -> IO Text
createTicket =
  fmap Text.pack . getStdRandom . randomRSequence ( '0', '9' )


--------------------------------------------------------------------------------
createClientJwt :: NominalDiffTime -> Secret -> Text -> Text -> IO Text
createClientJwt seconds (Secret signer) clientTicket clientId =
  withTime <$> Time.getCurrentTime
  where
    withTime :: UTCTime -> Text
    withTime time =
      createJwt (expirationDate time) signer clientClaims

    expirationDate :: UTCTime -> Maybe NumericDate
    expirationDate =
      JWT.numericDate . (seconds +) . utcTimeToPOSIXSeconds

    clientClaims :: [ ( Text, JSON.Value ) ]
    clientClaims =
      [ ( "tik", JSON.String clientTicket )
      , ( "cid", JSON.String clientId )
      ]


--------------------------------------------------------------------------------
createJwt :: Maybe NumericDate -> Signer -> [ ( Text, JSON.Value ) ] -> Text
createJwt expirationDate signer unregClaims =
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
verifyClientJwt :: Secret -> Text -> Maybe ( Text, Text )
verifyClientJwt (Secret signer) =
  getClientClaims <=< JWT.decodeAndVerifySignature signer


--------------------------------------------------------------------------------
getClientClaims :: JWT VerifiedJWT -> Maybe ( Text, Text )
getClientClaims jwt =
  (,) <$> claimField "tik" <*> claimField "cid"
  where
    claimField :: FromJSON a => Text -> Maybe a
    claimField fieldName =
      Map.lookup fieldName (unClaimsMap unregClaims) >>= maybeFromJSON

    unregClaims :: ClaimsMap
    unregClaims =
      unregisteredClaims $ JWT.claims jwt

    maybeFromJSON :: FromJSON a => Value -> Maybe a
    maybeFromJSON value =
      case JSON.fromJSON value of
        Success x -> Just x
        _         -> Nothing
