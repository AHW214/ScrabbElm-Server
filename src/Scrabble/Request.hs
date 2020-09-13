module Scrabble.Request
  ( app,
  )
where

import Network.HTTP.Types
import Network.Wai
import RIO
import qualified RIO.ByteString.Lazy as BL
import Scrabble.App
import Scrabble.Authentication

app ::
  ( HasLogFunc env,
    HasPendingClients env
  ) =>
  Request ->
  (Response -> IO ResponseReceived) ->
  RIO env ResponseReceived
app request respond = do
  (status, headers, body) <-
    case (requestMethod request, pathInfo request) of
      ("GET", ["auth"]) -> do
        logInfo "Client requested authentication!"

        let clientId = "test"

        jwt <-
          createClientJWT $
            ClientJWTParams
              { jwtClientId = clientId,
                jwtExpireIn = 1000,
                jwtSecret = "secret"
              }

        addPendingClient clientId

        pure $ authResponse jwt
      _ -> pure unsupported

  liftIO $ respond $ responseLBS status headers body
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
