module Scrabble.Request
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent        (MVar, modifyMVar_)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Text                 (Text)
import           Network.HTTP.Types        (status200, status501)
import           Network.HTTP.Types.Header (hCacheControl, hContentType)
import           Network.Wai               (Application, requestMethod,
                                            responseLBS)

import           Scrabble.Server           (Server)

import qualified Data.Text.Lazy            as T
import qualified Data.Text.Lazy.Encoding   as T

import qualified Scrabble.Authentication   as Auth
import qualified Scrabble.Server           as Server


--------------------------------------------------------------------------------
app :: MVar Server -> Application
app mServer request response = do
  ( status, headers, text ) <-
    case requestMethod request of
      "GET" -> do
        let getHeaders =
              [ ( hContentType, "text/plain" )
              , ( hCacheControl, "no-cache" )
              ]

        ticket <- Auth.ticket 10
        jwt <- Auth.jwt "CHANGE ME LATER ALSO ADD CONFIG" ticket 1

        modifyMVar_ mServer $ pure . Server.createPendingClient ticket

        pure ( status200, getHeaders, txtToBsl jwt )

      _ ->
        let unsupportedHeaders = [ ( hContentType, "text/plain" ) ] in
        pure ( status501, unsupportedHeaders, "Operation unsupported" )

  response $ responseLBS status headers text


--------------------------------------------------------------------------------
txtToBsl :: Text -> ByteString
txtToBsl = T.encodeUtf8 . T.fromStrict
