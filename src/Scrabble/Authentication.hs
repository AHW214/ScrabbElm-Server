module Scrabble.Authentication
  ( ClientJWT,
    ClientJWTParams (..),
    Secret,
    clientIdFromJWT,
    createClientJWT,
    createSecret,
    encodeClientJWT,
  )
where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import RIO hiding (exp)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import RIO.Time
import Scrabble.Client (Client)
import Scrabble.Common (ID)
import Web.JWT

newtype ClientJWT = ClientJWT Text

data ClientJWTParams = ClientJWTParams
  { jwtClientId :: !(ID Client),
    jwtExpireIn :: !Integer,
    jwtSecret :: !Secret
  }

newtype Secret = Secret Signer

instance IsString Secret where
  fromString = createSecret . fromString

instance FromJSON Secret where
  parseJSON = fmap createSecret . parseJSON

createSecret :: Text -> Secret
createSecret = Secret . hmacSecret

clientIdFromJWT :: Secret -> Text -> Maybe (ID Client)
clientIdFromJWT (Secret signer) =
  getClientId <=< decodeAndVerifySignature signer
  where
    getClientId :: JWT VerifiedJWT -> Maybe (ID Client)
    getClientId =
      maybeFromJSON
        <=< Map.lookup "cid"
          . unClaimsMap
          . unregisteredClaims
          . claims

    maybeFromJSON :: FromJSON a => JSON.Value -> Maybe a
    maybeFromJSON value =
      case fromJSON value of
        Success x ->
          Just x
        _ ->
          Nothing

encodeClientJWT :: ClientJWT -> BL.ByteString
encodeClientJWT (ClientJWT text) =
  BL.fromStrict $ Text.encodeUtf8 text

createClientJWT :: MonadIO m => ClientJWTParams -> m ClientJWT
createClientJWT clientJwtParams =
  withTime <$> getCurrentTime
  where
    withTime :: UTCTime -> ClientJWT
    withTime currentTime =
      ClientJWT $ createJWT (expirationDate currentTime) signer clientClaims

    expirationDate :: UTCTime -> Maybe NumericDate
    expirationDate =
      numericDate . (expireInDiffTime +) . utcTimeToPOSIXSeconds

    expireInDiffTime :: NominalDiffTime
    expireInDiffTime = fromInteger $ expireIn `div` 1000

    clientClaims :: [(Text, JSON.Value)]
    clientClaims =
      [ ("cid", toJSON clientId)
      ]

    ClientJWTParams
      { jwtClientId = clientId,
        jwtExpireIn = expireIn,
        jwtSecret = Secret signer
      } = clientJwtParams

createJWT :: Maybe NumericDate -> Signer -> [(Text, JSON.Value)] -> Text
createJWT expirationDate signer unregClaims =
  encodeSigned signer jwtHeader jwtClaims
  where
    jwtHeader :: JOSEHeader
    jwtHeader =
      mempty
        { typ = Just "JWT",
          alg = Just HS256
        }

    jwtClaims :: JWTClaimsSet
    jwtClaims =
      mempty
        { iss = stringOrURI "ScrabbElm-Server",
          sub = stringOrURI "ScrabbElm-Client",
          exp = expirationDate,
          unregisteredClaims =
            ClaimsMap $ Map.fromList unregClaims
        }
