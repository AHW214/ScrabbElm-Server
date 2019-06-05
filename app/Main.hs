{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=), parseJSON, toEncoding, pairs, withObject, decodeStrict, encode)
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

data Player
  = Player
      { name :: Text
      , score :: Int
      } deriving Generic

instance ToJSON Player where

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

encodeTextStrict :: ToJSON a => a -> Text
encodeTextStrict =
  toStrict . decodeUtf8 . encode

clientToPlayer :: WS.Client -> Player
clientToPlayer (name, _) =
  Player
    { name = name
    , score = 0
    }

onConnect :: WS.Client -> WS.ServerState -> IO ()
onConnect _ clients =
  case clients of
    c1:c2:[] -> do
      WS.send (playerMsg $ clientToPlayer c1) c2
      WS.send (playerMsg $ clientToPlayer c2) c1
    _ ->
        return ()
  where
      playerMsg player = "{ \"eventType\": \"playerJoined\", \"data\": { \"player\": " <> (encodeTextStrict player) <> " } }"

onDisconnect :: WS.Client -> WS.ServerState -> IO ()
onDisconnect (name, _) clients =
  case clients of
    c:[] ->
      WS.close ("Player " <> name <> " disconnected! Guess you win?") c
    _ ->
        return ()

onMessage :: Text -> WS.Client -> WS.ServerState -> IO ()
onMessage text client clients =
  case readEventType text of
    Just (Message ev)
      | ev == "startGame" -> do
          bag <- Tile.shuffleBag $ take 14 Tile.defaultBag
          WS.send (bagMsg bag) client
          WS.broadcast (bagMsg $ drop 7 bag) $ filter ((/=) (fst client) . fst) clients
      | elem ev events ->
          WS.broadcast text $ filter ((/=) (fst client) . fst) clients
      | otherwise ->
          T.putStrLn ("'" <> fst client <>  "' sent message with invalid event: \"" <> pack ev <> "\"")
    Nothing ->
      T.putStrLn ("'" <> fst client <>  "' sent invalid message: \"" <> text <> "\"")
  where
    bagMsg bag = "{ \"eventType\": \"startGame\", \"data\": { \"bag\": " <> (encodeTextStrict bag) <> " } }"

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

  let handlers = WS.Handlers onConnect onMessage onDisconnect
  wsApp <- WS.initApp handlers tickets

  run port $ websocketsOr WS.opts wsApp rqApp