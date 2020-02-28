{-# LANGUAGE OverloadedStrings #-}

module Request
  ( app
  ) where

import Control.Concurrent (MVar, modifyMVar_)
import Network.HTTP.Types (status200, status501)
import Network.HTTP.Types.Header (ResponseHeaders, hContentType, hCacheControl)
import Network.Wai (Application, requestMethod, responseLBS)
import Server (Server)
import qualified Server
import qualified Tickets

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

        return ( status200, getHeaders, ticket )

      _ ->
        let unsupportedHeaders = [ ( hContentType, "text/plain" ) ] in
        return ( status501, unsupportedHeaders, "Operation unsupported" )

  response $ responseLBS status headers text
