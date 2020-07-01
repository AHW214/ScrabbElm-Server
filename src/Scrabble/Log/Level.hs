module Scrabble.Log.Level
  ( LogLevel (..)
  , toTag
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson (FromJSON (parseJSON))
import           Data.Text  (Text)
import           Text.Read  (readMaybe)

import qualified Data.Aeson as JSON
import qualified Data.Text  as Text


--------------------------------------------------------------------------------
data LogLevel
  = LogError
  | LogWarning
  | LogInfo
  | LogDebug
  deriving (Eq, Ord, Read)


--------------------------------------------------------------------------------
instance FromJSON LogLevel where
  parseJSON = JSON.withText "LogLevel" $ \text ->
    case fromText text of
      Just logLevel ->
        pure logLevel

      _ ->
        fail "invalid LogLevel"


--------------------------------------------------------------------------------
fromText :: Text -> Maybe LogLevel
fromText = readMaybe . Text.unpack . Text.toTitle


--------------------------------------------------------------------------------
toTag :: LogLevel -> Text
toTag = \case
  LogError   -> "ERROR"
  LogWarning -> "WARN"
  LogInfo    -> "INFO"
  LogDebug   -> "DEBUG"
