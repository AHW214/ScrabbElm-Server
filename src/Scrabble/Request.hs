module Scrabble.Request
  ( app,
  )
where

import Network.HTTP.Types (Status, status200, status501)
import Network.HTTP.Types.Header (ResponseHeaders, hCacheControl, hContentType)
import Network.Wai (Application, requestMethod, responseLBS)
import RIO
import qualified RIO.ByteString.Lazy as BL

app :: Application
app request respond =
  let (status, headers, body) =
        case requestMethod request of
          "GET" -> get
          _ -> unsupported
   in respond $ responseLBS status headers body
  where
    get :: (Status, ResponseHeaders, BL.ByteString)
    get =
      ( status200,
        [ (hContentType, "text/plain; charset=utf-8"),
          (hCacheControl, "no-cache")
        ],
        "meme"
      )

    unsupported :: (Status, ResponseHeaders, BL.ByteString)
    unsupported =
      ( status501,
        [(hContentType, "text/plain")],
        "Operation unsupported."
      )
