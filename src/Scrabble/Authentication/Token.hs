module Scrabble.Authentication.Token
  ( Decoded,
    Encoded,
    Token,
    Secret,
    create,
    decodeFromText,
    retrieveClaim,
    toLazyByteString,
  )
where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as JSON
import RIO hiding (exp)
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import RIO.Time
import Scrabble.Common
import Web.JWT

data Token a where
  Decoded :: JWT VerifiedJWT -> Token Decoded
  Encoded :: Text -> Token Encoded

data Decoded

data Encoded

newtype Secret = Secret Signer

retrieveClaim :: forall a. FromJSON a => Text -> Token Decoded -> Maybe a
retrieveClaim claim (Decoded jwt) = fromJWT jwt
  where
    fromJWT :: JWT VerifiedJWT -> Maybe a
    fromJWT =
      maybeFromJSON
        <=< Map.lookup claim
          . unClaimsMap
          . unregisteredClaims
          . claims

toLazyByteString :: Token Encoded -> BL.ByteString
toLazyByteString (Encoded text) =
  BL.fromStrict $ Text.encodeUtf8 text

decodeFromText :: Signer -> Text -> Maybe (Token Decoded)
decodeFromText signer =
  fmap Decoded . decodeAndVerifySignature signer

create :: Maybe NominalDiffTime -> Secret -> [(Text, JSON.Value)] -> Token Encoded
create expirationDate (Secret signer) unregisteredClaims =
  Encoded $ encodeSigned signer jwtHeader jwtClaims
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
          exp = numericDate =<< expirationDate,
          unregisteredClaims =
            ClaimsMap $ Map.fromList unregisteredClaims
        }
