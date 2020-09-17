module Scrabble.Request
  ( app,
  )
where

import Network.HTTP.Types
import Network.Wai
import RIO
import Scrabble.Authentication.Client

app ::
  ( HasClientAuth env,
    HasLogFunc env
  ) =>
  Request ->
  (Response -> IO ResponseReceived) ->
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
