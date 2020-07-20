module Scrabble.Request
  ( app
  ) where


--------------------------------------------------------------------------------
import           Data.ByteString.Lazy      (ByteString)
import           Data.Text                 (Text)
import           Network.HTTP.Types        (status200, status501)
import           Network.HTTP.Types.Header (hCacheControl, hContentType)
import           Network.Wai               (Application, requestMethod,
                                            responseLBS)

import           Scrabble.Types            (Event (..), EventQueue, Gateway,
                                            Model (..))

import qualified Control.Concurrent.STM    as STM
import qualified Data.Text.Lazy            as Text
import qualified Data.Text.Lazy.Encoding   as Text


--------------------------------------------------------------------------------
app :: EventQueue Gateway -> Application
app gatewayQueue request respond = do
  ( status, headers, text ) <-
    case requestMethod request of
      "GET" -> do
        let getHeaders =
              [ ( hContentType, "text/plain; charset=utf-8" )
              , ( hCacheControl, "no-cache" )
              ]

        jwt <- STM.atomically $ do
            jwtQueue <- STM.newTBQueue 256 -- todo
            emit gatewayQueue $ GatewayCreateJWT jwtQueue
            STM.readTBQueue jwtQueue

        pure ( status200, getHeaders, txtToBsl jwt )

      _ ->
        let unsupportedHeaders = [ ( hContentType, "text/plain" ) ] in
        pure ( status501, unsupportedHeaders, "Operation unsupported" )

  respond $ responseLBS status headers text


--------------------------------------------------------------------------------
txtToBsl :: Text -> ByteString
txtToBsl = Text.encodeUtf8 . Text.fromStrict
