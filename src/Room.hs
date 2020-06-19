module Room
  ( Room (..)
  , addPlayer
  , empty
  , hasPlayerTag
  , maxCapacity
  , new
  , removePlayer
  , switchTurn
  ) where


--------------------------------------------------------------------------------
import           Data.Aeson         (FromJSON, ToJSON, withObject, (.=), (.:))
import qualified Data.Aeson         as JSON
import qualified Data.List          as List
import qualified Data.Maybe         as Maybe
import           Data.Text          (Text)
import           Network.WebSockets (Connection)


--------------------------------------------------------------------------------
import           Player (Player)
import qualified Player


--------------------------------------------------------------------------------
data Room
  = Room
      { capacity :: Int
      , name     :: Text
      , players  :: [ Player ]
      , playing  :: Maybe Player
      , roomId   :: Int
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
    , name     = ""
    , players  = []
    , playing  = Nothing
    , roomId   = 0
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
hasPlayerTag :: Text -> Room -> Bool
hasPlayerTag name =
  List.any (Player.hasName name) . players


--------------------------------------------------------------------------------
addPlayer :: Text -> Connection -> Room -> Room
addPlayer playerName conn room

    player = Player.new playerName client
    newRoom = room { players = player : players room }


--------------------------------------------------------------------------------
removePlayer :: Text -> Room -> Maybe Room
removePlayer playerName room@(Room { players }) =
  case ( players, newPlayers ) of
    ( _:[], [] ) ->
      Nothing

    _ ->
      Just $ room { players = newPlayers }
  where
    newPlayers = filter (not . Player.hasName playerName) players


--------------------------------------------------------------------------------
switchTurn :: Player -> Room -> Room
switchTurn player room =
  room { playing = Just player }
