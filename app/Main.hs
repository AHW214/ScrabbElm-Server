{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Map.Strict as Map
import Data.Aeson (FromJSON, (.:), parseJSON, withObject, decodeStrict, encode)
import Control.Concurrent (newMVar)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.WebSockets (websocketsOr)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Data.Text.IO as T

import qualified WebSockets as WS
import qualified Requests as RQ
import qualified Tickets
import qualified Tile

type Event
  = String

data Message
  = Message Event

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Message
    <$> v .: "eventType"

defaultPort :: Int
defaultPort = 3000

events :: [ Event ]
events =
  [ "exchanged"
  , "placed"
  , "passed"
  , "endGame"
  ]

readEventType :: Text -> Maybe Message
readEventType =
  decodeStrict . encodeUtf8

messageHandler :: WS.Handler
messageHandler text client clients =
  case readEventType text of
    Just (Message ev)
      | ev == "startGame" -> do
          bag <- Tile.shuffleBag Tile.defaultBag
          WS.send (bagMsg bag) client
          WS.broadcast (bagMsg $ drop 7 bag) $ Map.filter ((/=) (fst client) . fst) clients
      | elem ev events ->
          WS.broadcast text $ Map.filter ((/=) (fst client) . fst) clients
      | otherwise ->
          T.putStrLn ("'" <> fst client <>  "' sent message with invalid event: \"" <> pack ev <> "\"")
    Nothing ->
      T.putStrLn ("'" <> fst client <>  "' sent invalid message: \"" <> text <> "\"")
  where
    bagMsg bag = "{ \"eventType\": \"startGame\", \"data\": { \"bag\": " <> (toStrict $ decodeUtf8 $ encode bag) <> " } }"

assignPort :: [ String ] -> Int
assignPort args =
  fromMaybe defaultPort (listToMaybe args >>= readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let port = assignPort args

  Prelude.putStrLn ("Listening on port " ++ show port)

  tickets <- newMVar Tickets.empty
  let rqApp = simpleCors $ RQ.app tickets
  wsApp <- WS.initApp messageHandler tickets

  run port $ websocketsOr WS.opts wsApp rqApp