{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Data.Text.IO as TIO
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson (FromJSON, (.:), parseJSON, withObject, decodeStrict)

import Server (Handler, start, send, broadcast)
import Tile

type Event
  = String

data Message
  = Message Event

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Message
    <$> v .: "eventType"

events :: [ String ]
events =
  [ "exchanged"
  , "placed"
  , "passed"
  , "endGame"
  ]

messageHandler :: Handler
messageHandler text client clients =
  case decodeStrict (encodeUtf8 text) of
    Just (Message ev) | elem ev events ->
      broadcast text clients
    Nothing ->
      TIO.putStrLn ("'" <> fst client <>  "' sent invalid message: \"" <> text <> "\"")

main :: IO ()
main =
  Server.start "127.0.0.1" 3000 messageHandler
