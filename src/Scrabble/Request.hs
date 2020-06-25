module Scrabble.Request
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent        (MVar, ThreadId, forkIO, modifyMVar,
                                            modifyMVar_, threadDelay)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Text                 (Text)
import           Data.Time.Clock           (NominalDiffTime)
import           Network.HTTP.Types        (status200, status501)
import           Network.HTTP.Types.Header (hCacheControl, hContentType)
import           Network.Wai               (Application, requestMethod,
                                            responseLBS)

import           Scrabble.Server           (Server, PendingParams (..))

import qualified Data.Aeson                as JSON
import qualified Data.Text.Lazy            as Text
import qualified Data.Text.Lazy.Encoding   as Text
import qualified Data.Time.Clock           as Time

import qualified Scrabble.Authentication   as Auth
import qualified Scrabble.Server           as Server


--------------------------------------------------------------------------------
app :: MVar Server -> Application
app mServer request response = do
  ( status, headers, text ) <-
    case requestMethod request of
      "GET" -> do
        let getHeaders =
              [ ( hContentType, "text/plain; charset=utf-8" )
              , ( hCacheControl, "no-cache" )
              ]

        pendingClientTicket <- Auth.ticket 10

        PendingParams
          { pendingAuthKey
          , pendingClientId
          , pendingTimeout
          } <- modifyMVar mServer $
                pure . Server.createPendingClient pendingClientTicket

        jwt <- Auth.jwt pendingTimeout pendingAuthKey
                [ ( "tik", JSON.String pendingClientTicket )
                , ( "cid", JSON.String pendingClientId )
                ]

        let microseconds = nominalToMicroseconds pendingTimeout

        timeOutPendingClient microseconds pendingClientId mServer

        pure ( status200, getHeaders, txtToBsl jwt )

      _ ->
        let unsupportedHeaders = [ ( hContentType, "text/plain" ) ] in
        pure ( status501, unsupportedHeaders, "Operation unsupported" )

  response $ responseLBS status headers text


--------------------------------------------------------------------------------
timeOutPendingClient :: Int -> Text -> MVar Server -> IO ThreadId
timeOutPendingClient timeout clientId mServer =
  forkIO $ threadDelay timeout
    >> modifyMVar_ mServer (pure . Server.removePendingClient clientId)


--------------------------------------------------------------------------------
nominalToMicroseconds :: NominalDiffTime -> Int
nominalToMicroseconds = floor . (1e6 *) . Time.nominalDiffTimeToSeconds


--------------------------------------------------------------------------------
txtToBsl :: Text -> ByteString
txtToBsl = Text.encodeUtf8 . Text.fromStrict
