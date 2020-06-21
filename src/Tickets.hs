module Tickets
  ( Ticket
  , empty
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.Text     (Text)
import qualified Data.Text     as T
import           System.Random (getStdRandom)


--------------------------------------------------------------------------------
import           Random (randomRSequence)


--------------------------------------------------------------------------------
type Ticket =
  Text


--------------------------------------------------------------------------------
empty :: [ Ticket ]
empty = []


--------------------------------------------------------------------------------
new :: Int -> IO Ticket
new =
  fmap T.pack . getStdRandom . randomRSequence ( '0', '9' )
