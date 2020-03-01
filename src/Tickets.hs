module Tickets
  ( Ticket
  , empty
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.ByteString.Lazy.Char8 (ByteString, pack)
import           System.Random              (newStdGen, randomRs)


--------------------------------------------------------------------------------
type Ticket =
  ByteString


--------------------------------------------------------------------------------
empty :: [ Ticket ]
empty = []


--------------------------------------------------------------------------------
new :: Int -> IO Ticket
new size =
  pack . take size . randomRs ('0', '9') <$> newStdGen
