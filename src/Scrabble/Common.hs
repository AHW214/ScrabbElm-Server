module Scrabble.Common
  ( ID (..)
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson  (FromJSON (parseJSON), ToJSON (toEncoding, toJSON))
import           Data.String (IsString (fromString))
import           Data.Text   (Text)


--------------------------------------------------------------------------------
data ID a = ID Text
  deriving (Eq, Ord)


--------------------------------------------------------------------------------
instance FromJSON (ID a) where
  parseJSON = fmap ID . parseJSON


--------------------------------------------------------------------------------
instance ToJSON (ID a) where
  toJSON     (ID text) = toJSON text
  toEncoding (ID text) = toEncoding text


--------------------------------------------------------------------------------
instance IsString (ID a) where
  fromString = ID . fromString
