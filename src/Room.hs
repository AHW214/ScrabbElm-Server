module Room
  ( Room (..)
  , addPlayer
  , empty
  , getPlayer
  , hasPlayerTag
  , inGame
  , isEmpty
  , isFull
  , maxCapacity
  , new
  , removePlayer
  , switchTurn
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson         (FromJSON, ToJSON, withObject, (.=), (.:))
import qualified Data.Aeson         as JSON
import           Data.Map           (Map)
import qualified Data.Map           as Map
import qualified Data.Maybe         as Maybe
import           Data.Text          (Text)
import           Prelude            hiding (id)


--------------------------------------------------------------------------------
import           Player (Player (Player))
import qualified Player


--------------------------------------------------------------------------------
data Room
  = Room
      { capacity :: Int
      , id       :: Int
      , name     :: Text
      , players  :: Map Text Player
      , playing  :: Maybe Player
      }


--------------------------------------------------------------------------------
instance ToJSON Room where
  toJSON room =
    JSON.object
      [ "name"        .= name room
      , "capacity"    .= capacity room
      , "numPlayers"  .= numPlayers room
      , "gameStarted" .= inGame room
      ]

  toEncoding room =
    JSON.pairs
      $  "name"        .= name room
      <> "capacity"    .= capacity room
      <> "numPlayers"  .= numPlayers room
      <> "gameStarted" .= inGame room


--------------------------------------------------------------------------------
instance FromJSON Room where
  parseJSON = withObject "Room" $ \v -> do
    name     <- v .: "name"
    capacity <- v .: "capacity"

    return $ empty { name, capacity }


--------------------------------------------------------------------------------
maxCapacity :: Int
maxCapacity = 4


--------------------------------------------------------------------------------
empty :: Room
empty =
  Room
    { capacity = maxCapacity
    , id       = 0
    , name     = ""
    , players  = Map.empty
    , playing  = Nothing
    }


--------------------------------------------------------------------------------
new :: Text -> Int -> Room
new name capacity =
  empty { capacity, name }


--------------------------------------------------------------------------------
inGame :: Room -> Bool
inGame = Maybe.isJust . playing


--------------------------------------------------------------------------------
numPlayers :: Room -> Int
numPlayers = length . players


--------------------------------------------------------------------------------
isFull :: Room -> Bool
isFull room =
  numPlayers room >= capacity room


--------------------------------------------------------------------------------
isEmpty :: Room -> Bool
isEmpty room =
  numPlayers room <= 0


--------------------------------------------------------------------------------
getPlayer :: Text -> Room -> Maybe Player
getPlayer tag =
  Map.lookup tag . players


--------------------------------------------------------------------------------
hasPlayerTag :: Text -> Room -> Bool
hasPlayerTag tag =
  Map.member tag . players


--------------------------------------------------------------------------------
addPlayer :: Player -> Room -> Room
addPlayer player@Player { Player.name } room@Room { players } =
  room { players = Map.insert name player players }


--------------------------------------------------------------------------------
removePlayer :: Player -> Room -> Room
removePlayer Player { Player.name } room@Room { players } =
  room { players = Map.delete name players }


--------------------------------------------------------------------------------
switchTurn :: Player -> Room -> Room
switchTurn player room =
  room { playing = Just player }
