module Scrabble.Authentication.Token
  ( Decoded,
    Encoded,
    Token,
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
import Scrabble.Common
import Web.JWT

data Decoded

data Encoded

data Token a where
  Decoded :: JWT VerifiedJWT -> Token Decoded
  Encoded :: Text -> Token Encoded

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

create :: Maybe NumericDate -> Signer -> [(Text, JSON.Value)] -> Token Encoded
create expirationDate signer unregisteredClaims =
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
          exp = expirationDate,
          unregisteredClaims =
            ClaimsMap $ Map.fromList unregisteredClaims
        }
