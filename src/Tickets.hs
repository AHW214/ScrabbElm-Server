{-# LANGUAGE OverloadedStrings #-}

module Tickets
  ( Ticket
  , empty
  , new
  ) where

import System.Random (newStdGen, randomRs)
import Data.ByteString.Lazy.Char8 (ByteString, pack)

type Ticket =
  ByteString

empty :: [ Ticket ]
empty = []

new :: Int -> IO Ticket
new size =
  newStdGen >>= pure . pack . take size . randomRs ('0', '9')