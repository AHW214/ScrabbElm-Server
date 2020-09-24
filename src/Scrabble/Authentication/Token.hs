-- | Operations with JSON Web Tokens.
module Scrabble.Authentication.Token
  ( Decoded,
    Encoded,
    Token,
    Secret,
    create,
    createFixedSizeSecret,
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

-- | A JSON Web Token.
data Token a where
  -- | A token decoded into a datatype representation.
  Decoded :: JWT VerifiedJWT -> Token Decoded
  -- | A token encoded as text.
  Encoded :: Text -> Token Encoded

-- | Type for labeling a token as decoded.
data Decoded

-- | Type for labeling a token as encoded.
data Encoded

-- | An HMAC secret key for signing tokens.
newtype Secret = Secret Signer

instance IsString Secret where
  fromString = createSecret . fromString

createFixedSizeSecret :: Text -> Either Text Secret
createFixedSizeSecret text =
  if Text.length text == secretSizeBytes
    then Right $ createSecret text
    else
      Left $
        "Secret must be "
          <> textDisplay (8 * secretSizeBytes)
          <> " bits ("
          <> textDisplay secretSizeBytes
          <> " bytes) long"

-- | Create an HMAC secret from text.
createSecret :: Text -> Secret
createSecret = Secret . hmacSecret

secretSizeBytes :: Int
secretSizeBytes = 32

-- | Retrieve an unregistered claim from a decoded token.
retrieveClaim ::
  forall a.
  FromJSON a =>
  -- | The name of the claim to retrieve.
  Text ->
  -- | The decoded token whose claims will be searched.
  Token Decoded ->
  -- | The value of the claim, if found.
  Maybe a
retrieveClaim claim (Decoded jwt) = fromJWT jwt
  where
    fromJWT :: JWT VerifiedJWT -> Maybe a
    fromJWT =
      maybeFromJSON
        <=< Map.lookup claim
          . unClaimsMap
          . unregisteredClaims
          . claims

-- | Create a lazy bytestring from a text-encoded token.
toLazyByteString :: Token Encoded -> BL.ByteString
toLazyByteString (Encoded text) =
  BL.fromStrict $ Text.encodeUtf8 text

-- | Decode a token from text.
decodeFromText ::
  -- | The HMAC secret used to sign the token.
  Secret ->
  -- | The text encoding the token.
  Text ->
  -- | The token, if decoding was successful.
  Maybe (Token Decoded)
decodeFromText (Secret signer) =
  fmap Decoded . decodeAndVerifySignature signer

-- | Create a text-encoded token.
create ::
  -- | An optional expiration date.
  Maybe NominalDiffTime ->
  -- | An HMAC secret for signing the token.
  Secret ->
  -- | A map of unregistered claims to include in the token.
  [(Text, JSON.Value)] ->
  -- | The encoded token.
  Token Encoded
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
