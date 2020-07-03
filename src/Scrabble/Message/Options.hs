module Scrabble.Message.Options
  ( jsonOptions
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson (Options (..), SumEncoding (..))

import qualified Data.Aeson as JSON


--------------------------------------------------------------------------------
jsonOptions :: Options
jsonOptions =
  JSON.defaultOptions { sumEncoding }
  where
    sumEncoding :: SumEncoding
    sumEncoding = TaggedObject
      { tagFieldName = "messageType"
      , contentsFieldName = "messageData"
      }
