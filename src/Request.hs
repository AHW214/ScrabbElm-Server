module Request
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent        (MVar, modifyMVar_)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Network.HTTP.Types        (status200, status501)
import           Network.HTTP.Types.Header (hCacheControl, hContentType)
import           Network.Wai               (Application, requestMethod,
                                            responseLBS)


--------------------------------------------------------------------------------
import           Server (Server)
import qualified Server
import qualified Tickets


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

        return ( status200, getHeaders, byteStringLazy ticket )

      _ ->
        let unsupportedHeaders = [ ( hContentType, "text/plain" ) ] in
        return ( status501, unsupportedHeaders, "Operation unsupported" )

  response $ responseLBS status headers text


--------------------------------------------------------------------------------
byteStringLazy :: Text -> ByteString
byteStringLazy = TL.encodeUtf8 . TL.fromStrict
