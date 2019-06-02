{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson (FromJSON, (.:), parseJSON, withObject, decodeStrict)
import Control.Concurrent (newMVar)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Data.Text.IO as T

import qualified WebSockets as WS
import qualified Requests as RQ
import qualified Tickets
import Tile

type Event
  = String

data Message
  = Message Event

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Message
    <$> v .: "eventType"

events :: [ Event ]
events =
  [ "exchanged"
  , "placed"
  , "passed"
  , "endGame"
  ]

messageHandler :: WS.Handler
messageHandler text client clients =
  case decodeStrict (encodeUtf8 text) of
    Just (Message ev) | elem ev events ->
      WS.broadcast text clients
    Nothing ->
      T.putStrLn ("'" <> fst client <>  "' sent invalid message: \"" <> text <> "\"")

main :: IO ()
main = do
  let port = 3000
  Prelude.putStrLn ("Listening on port " ++ show port)

  tickets <- newMVar Tickets.empty
  let rqApp = simpleCors $ RQ.app tickets
  wsApp <- WS.initApp messageHandler tickets

  run port $ websocketsOr WS.opts wsApp rqApp
