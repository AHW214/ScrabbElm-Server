module Scrabble.Authentication
  ( Crypt
  , Plain
  , cryptToBSL
  , new
  ) where


--------------------------------------------------------------------------------
import           Crypto.Simple.CTR     (encrypt)
import           System.Random         (getStdRandom)

import           Scrabble.Random       (randomRSequence)

import qualified Data.ByteString.Char8 as BSS
import qualified Data.ByteString.Lazy  as BSL


--------------------------------------------------------------------------------
newtype Plain = Plain BSS.ByteString


--------------------------------------------------------------------------------
newtype Crypt = Crypt BSS.ByteString


--------------------------------------------------------------------------------
new :: BSS.ByteString -> Int -> IO ( Plain, Crypt )
new key len = do
  plain <- BSS.pack <$> getStdRandom (randomRSequence ( '0', '9' ) len)
  crypt <- encrypt key plain
  pure ( Plain plain, Crypt crypt )


--------------------------------------------------------------------------------
cryptToBSL :: Crypt -> BSL.ByteString
cryptToBSL (Crypt crypt) = BSL.fromStrict crypt
