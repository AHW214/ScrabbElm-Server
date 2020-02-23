{-# LANGUAGE OverloadedStrings #-}

module Room
  ( Room(..)
  , Preview
  , new
  , addPlayer
  ) where

import Client (Client)
import qualified Client
import qualified Data.List as List
import Data.Text (Text)
import Player (Player)
import qualified Player
import Prelude hiding (id)

data Room
  = Room
      { id :: Int
      , name :: Text
      , capacity :: Int
      , players :: [ Player ]
      , playing :: Maybe Player
      }

data Preview
  = Preview
      { roomName :: Text
      , roomCapacity :: Int
      , numPlayers :: Int
      , gameStarted :: Bool
      }

maxCapacity :: Int
maxCapacity = 4

empty :: Room
empty =
  Room
    { id = 0
    , name = ""
    , capacity = maxCapacity
    , players = []
    , playing = Nothing
    }

new :: Text -> Int -> Either Text Room
new name capacity =
  if capacity > maxCapacity then
    Left "Capacity is too large"
  else
    Right $ empty { name = name, capacity = capacity }

inGame :: Room -> Bool
inGame (Room { playing = p }) =
  case p of
    Nothing ->
      False

    _ ->
      True

isFull :: Room -> Bool
isFull (Room { capacity = c, players = ps }) =
  length ps >= c

hasPlayerTag :: Text -> Room -> Bool
hasPlayerTag name (Room { players = ps }) =
  List.any (Player.hasName name) ps

addPlayer :: Text -> Client -> Room -> Either String ( Room, Player )
addPlayer playerName client room
  | inGame room =
      Left "Game already started"
  | isFull room =
      Left "Room is full"
  | hasPlayerTag playerName room =
      Left "Player already in room"
  | otherwise =
      let
        player = Player.new playerName client
        newRoom = room { players = player : (players room) }
      in
        Right ( newRoom, player )

removePlayer :: Text -> Room -> Maybe Room
removePlayer playerName room@(Room { players = ps }) =
  case ( ps, newPlayers ) of
    ( _:[], [] ) ->
      Nothing

    _ ->
      Just $ room { players = newPlayers }
  where
    newPlayers = filter (not . Player.hasName playerName) ps

switchTurn :: Player -> Room -> Room
switchTurn player room =
  room { playing = Just player }

toPreview :: Room -> Preview
toPreview room@(Room { name = n, capacity = c, players = ps }) =
  Preview
    { roomName = n
    , roomCapacity = c
    , numPlayers = length ps
    , gameStarted = inGame room
    }
