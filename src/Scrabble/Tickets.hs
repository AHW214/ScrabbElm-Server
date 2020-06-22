module Scrabble.Tickets
  ( Ticket
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Text       (Text)
import           System.Random   (getStdRandom)

import           Scrabble.Random (randomRSequence)

import qualified Data.Text       as T


--------------------------------------------------------------------------------
type Ticket = Text


--------------------------------------------------------------------------------
new :: Int -> IO Ticket
new =
  fmap T.pack . getStdRandom . randomRSequence ( '0', '9' )
