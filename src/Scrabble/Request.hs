module Scrabble.Request
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent        (MVar, modifyMVar_)
import           Network.HTTP.Types        (status200, status501)
import           Network.HTTP.Types.Header (hCacheControl, hContentType)
import           Network.Wai               (Application, requestMethod,
                                            responseLBS)

import           Scrabble.Server           (Server)

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

        ( plain, crypt ) <- Auth.new "CHANGE ME" 10

        modifyMVar_ mServer $ pure . Server.createPendingClient plain

        pure ( status200, getHeaders, Auth.cryptToBSL crypt )

      _ ->
        let unsupportedHeaders = [ ( hContentType, "text/plain" ) ] in
        pure ( status501, unsupportedHeaders, "Operation unsupported" )

  response $ responseLBS status headers text
