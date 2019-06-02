{-# LANGUAGE OverloadedStrings #-}

module Requests
  ( app
  ) where

import Control.Concurrent (MVar, modifyMVar_)
import Network.Wai (Application, requestMethod, responseLBS)
import Network.HTTP.Types (status200, status501)
import Network.HTTP.Types.Header (hContentType)

import Tickets (Ticket, new)

app :: MVar [ Ticket ] -> Application
app tickets req res = do
  (code, text) <-
    case requestMethod req of
      "GET" -> do
        ticket <- Tickets.new 10
        modifyMVar_ tickets $ pure . (:) ticket
        return (status200, ticket)
      _ ->
        return (status501, "Goodbye.")

  res $ responseLBS code [ ( hContentType, "text/plain" ) ] text