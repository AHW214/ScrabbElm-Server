{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Scrabble.Message
  ( Inbound
  , JoinRoom (..)
  , MakeRoom (..)
  , Message (..)
  , Outbound
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson       (FromJSON (parseJSON), GFromJSON, Options
                                   (constructorTagModifier, fieldLabelModifier),
                                   ToJSON (toJSON), Value, Zero)
import           Data.Aeson.Types (Parser)
import           Data.Kind        (Type)
import           Data.Text        (Text)
import           GHC.Generics     (Generic, Rep)

-- import           Scrabble.Common     (ID)
import           Scrabble.Error   (Error)
-- import           Scrabble.Room    (Room)

import qualified Data.Aeson       as JSON
import qualified Data.Char        as Char
import qualified Data.List        as List


--------------------------------------------------------------------------------
class Communication a where
  data Message a :: Type


--------------------------------------------------------------------------------
data Inbound = Inbound


--------------------------------------------------------------------------------
instance Communication Inbound where
  data Message Inbound
    = JoinServer Text
    | MakeRoom MakeRoom
    | JoinRoom JoinRoom
    | LeaveRoom
    deriving Generic


--------------------------------------------------------------------------------
instance FromJSON (Message Inbound) where
  parseJSON = JSON.genericParseJSON messageEncodingOptions


--------------------------------------------------------------------------------
data MakeRoom = MR
  { mrRoomCapacity :: Int
  , mrRoomId       :: Text
  } deriving Generic


--------------------------------------------------------------------------------
instance FromJSON MakeRoom where
  parseJSON = parseInboundParameter


--------------------------------------------------------------------------------
data JoinRoom = JR
  { jrRoomId :: Text
  } deriving Generic


--------------------------------------------------------------------------------
instance FromJSON JoinRoom where
  parseJSON = parseInboundParameter


--------------------------------------------------------------------------------
parseInboundParameter
  :: (Generic a, GFromJSON Zero (Rep a))
  => Value
  -> Parser a
parseInboundParameter =
  JSON.genericParseJSON inboundParameterEncodingOptions


--------------------------------------------------------------------------------
inboundParameterEncodingOptions :: JSON.Options
inboundParameterEncodingOptions = JSON.defaultOptions
  { fieldLabelModifier = uncapitalize . removeLowercasePrefix
  }
  where
    removeLowercasePrefix :: String -> String
    removeLowercasePrefix = List.dropWhile Char.isLower


--------------------------------------------------------------------------------
data Outbound = Outbound


--------------------------------------------------------------------------------
instance Communication Outbound where
  data Message Outbound
    = JoinedServer [ Text ]
    | MadeRoom Text
    | RemovedRoom Text
    | JoinedRoom Text
    | LeftRoom
    | CausedError Error
    deriving Generic


--------------------------------------------------------------------------------
instance ToJSON (Message Outbound) where
  toJSON     = JSON.genericToJSON messageEncodingOptions
  toEncoding = JSON.genericToEncoding messageEncodingOptions


--------------------------------------------------------------------------------
messageEncodingOptions :: JSON.Options
messageEncodingOptions = JSON.defaultOptions
  { constructorTagModifier = uncapitalize
  }


--------------------------------------------------------------------------------
uncapitalize :: String -> String
uncapitalize = mapHead Char.toLower
  where
    mapHead :: (a -> a) -> [ a ] -> [ a ]
    mapHead f = \case
      x:xs -> f x : xs
      _    -> []
