module Scrabble.Authentication
  ( jwt
  , ticket
  ) where


--------------------------------------------------------------------------------
import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import           Data.Time.Clock       (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Prelude               hiding (exp)
import           System.Random         (getStdRandom)
import           Web.JWT               (Algorithm (HS256),
                                        ClaimsMap (ClaimsMap),
                                        JOSEHeader (..), JWTClaimsSet (..),
                                        NumericDate, Signer (HMACSecret))

import           Scrabble.Random       (randomRSequence)

import qualified Data.Aeson            as JSON
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import qualified Data.Time.Clock       as Time
import qualified Web.JWT               as JWT


--------------------------------------------------------------------------------
ticket :: Int -> IO Text
ticket =
  fmap T.pack . getStdRandom . randomRSequence ( '0', '9' )


--------------------------------------------------------------------------------
jwt :: ByteString -> Text -> NominalDiffTime -> IO Text
jwt key message seconds =
  JWT.encodeSigned secret header . makeClaims <$> Time.getCurrentTime
  where
    secret :: Signer
    secret = HMACSecret key

    header :: JOSEHeader
    header = mempty
      { typ = Just "JWT"
      , alg = Just HS256
      }

    makeClaims :: UTCTime -> JWTClaimsSet
    makeClaims now = mempty
      { iss = JWT.stringOrURI "ScrabbElm-Server"
      , sub = JWT.stringOrURI "ScrabbElm-Client"
      , exp = expirationDate now
      , unregisteredClaims =
          ClaimsMap $ Map.fromList
            [ ( "tik", JSON.String message ) ]
      }

    expirationDate :: UTCTime -> Maybe NumericDate
    expirationDate =
      JWT.numericDate . (seconds +) . utcTimeToPOSIXSeconds
