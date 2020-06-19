module Tickets
  ( Ticket
  , empty
  , new
  ) where


--------------------------------------------------------------------------------
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.Random              (getStdRandom)


--------------------------------------------------------------------------------
import           Random                     (randomRSequence)


--------------------------------------------------------------------------------
type Ticket =
  ByteString


--------------------------------------------------------------------------------
empty :: [ Ticket ]
empty = []


--------------------------------------------------------------------------------
new :: Int -> IO Ticket
new =
  fmap BS.pack . getStdRandom . randomRSequence ( '0', '9' )
