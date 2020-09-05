module Scrabble.Request
  ( app,
  )
where

--------------------------------------------------------------------------------

import qualified Control.Concurrent.STM as STM
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Network.HTTP.Types (status200, status501)
import Network.HTTP.Types.Header (hCacheControl, hContentType)
import Network.Wai
  ( Application,
    requestMethod,
    responseLBS,
  )
import Scrabble.Types
  ( Event (..),
    EventQueue,
    Gateway,
    Talk (..),
  )

--------------------------------------------------------------------------------
app :: EventQueue Gateway -> Application
app gatewayQueue request respond = do
  (status, headers, text) <-
    case requestMethod request of
      "GET" -> do
        let getHeaders =
              [ (hContentType, "text/plain; charset=utf-8"),
                (hCacheControl, "no-cache")
              ]

        jwtQueue <- STM.atomically $ do
          queue <- STM.newTBQueue 256 -- todo
          emit gatewayQueue $ GatewayCreateJWT queue
          pure queue

        jwt <- STM.atomically $ STM.readTBQueue jwtQueue

        pure (status200, getHeaders, txtToBsl jwt)
      _ ->
        let unsupportedHeaders = [(hContentType, "text/plain")]
         in pure (status501, unsupportedHeaders, "Operation unsupported")

  respond $ responseLBS status headers text

--------------------------------------------------------------------------------
txtToBsl :: Text -> ByteString
txtToBsl = Text.encodeUtf8 . Text.fromStrict
