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

import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL

import qualified Scrabble.Server           as Server
import qualified Scrabble.Tickets          as Tickets


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

        ticket <- Tickets.new 10
        modifyMVar_ mServer $ pure . Server.addPendingTicket ticket

        pure ( status200, getHeaders, byteStringLazy ticket )

      _ ->
        let unsupportedHeaders = [ ( hContentType, "text/plain" ) ] in
        pure ( status501, unsupportedHeaders, "Operation unsupported" )

  response $ responseLBS status headers text


--------------------------------------------------------------------------------
byteStringLazy :: Text -> ByteString
byteStringLazy = TL.encodeUtf8 . TL.fromStrict
