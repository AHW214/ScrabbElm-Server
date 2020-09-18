-- | Application for HTTP requests.
module Scrabble.Request
  ( app,
  )
where

import Network.HTTP.Types
import Network.Wai
import RIO
import Scrabble.Authentication.Client

-- | The application for handling HTTP requests.
-- Clients that perform a GET request for authentication will
-- receive a JWT. All other requests are unsupported.
app ::
  ( HasClientAuth env,
    HasLogFunc env
  ) =>
  -- | An HTTP request.
  Request ->
  -- | A function for issuing a response.
  (Response -> IO ResponseReceived) ->
  -- | The application.
  RIO env ResponseReceived
app request respond = do
  (status, headers, body) <-
    case (requestMethod request, pathInfo request) of
      ("GET", ["auth"]) -> do
        logInfo "Client requested authentication!"

        token <- cacheClient

        pure
          ( status200,
            [ (hContentType, "text/plain; charset=utf-8"),
              (hCacheControl, "no-cache")
            ],
            clientTokenToLazyByteString token
          )
      _ ->
        pure
          ( status501,
            [(hContentType, "text/plain")],
            "Operation unsupported."
          )

  liftIO $ respond $ responseLBS status headers body
