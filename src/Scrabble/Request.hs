module Scrabble.Request
  ( app,
  )
where

import Network.HTTP.Types
import Network.Wai
import RIO
import qualified RIO.ByteString.Lazy as BL
import Scrabble.Authentication

app :: Application
app request respond = do
  (status, headers, body) <-
    case (requestMethod request, pathInfo request) of
      ("GET", ["auth"]) -> do
        jwt <-
          createClientJWT $
            ClientJWTParams
              { jwtClientId = "test",
                jwtExpireIn = 1000,
                jwtSecret = "secret"
              }

        pure $ authResponse jwt
      _ -> pure unsupported

  respond $ responseLBS status headers body
  where
    authResponse :: ClientJWT -> (Status, ResponseHeaders, BL.ByteString)
    authResponse jwt =
      ( status200,
        [ (hContentType, "text/plain; charset=utf-8"),
          (hCacheControl, "no-cache")
        ],
        encodeClientJWT jwt
      )

    unsupported :: (Status, ResponseHeaders, BL.ByteString)
    unsupported =
      ( status501,
        [(hContentType, "text/plain")],
        "Operation unsupported."
      )
