-- Temporary module for reworking message and concurrency things --

module Scrabble.Things
  ( main
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM         (STM, TChan)
import           Control.Monad                  (forever, (<=<))
import           Data.Map                       (Map)
import           Data.Text                      (Text)
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets             (Connection, ServerApp)

import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.STM         as STM
import qualified Data.List                      as List
import qualified Data.Map                       as Map
import qualified Data.Maybe                     as Maybe
import qualified Network.HTTP.Types             as HTTP
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.WebSockets             as WS


--------------------------------------------------------------------------------
data Lobby = Lobby
  { lobbyChannel   :: TChan LobbyEvent
  , lobbyClients   :: Map Text (TChan ClientEvent)
  , lobbyRoomViews :: Map Text RoomView
  }


--------------------------------------------------------------------------------
data RoomView = RoomView
  { roomViewChannel   :: TChan RoomEvent
  , roomViewInGame    :: Bool
  , roomViewName      :: Text
  , roomViewOccupancy :: Int
  }


--------------------------------------------------------------------------------
data Room = Room
  { roomChannel :: TChan RoomEvent
  , roomName    :: Text
  , roomOwner   :: Client
  , roomPlayers :: Map Client Player
  , roomPlaying :: Maybe Client
  }


--------------------------------------------------------------------------------
data Player = Player
  { playerName  :: Text
  , playerScore :: Int
  }


--------------------------------------------------------------------------------
data Client = Client
  { clientChannel     :: TChan ClientEvent
  , clientConnection  :: Connection
  , clientId          :: Text
  , clientRoomChannel :: Maybe (TChan RoomEvent)
  }


--------------------------------------------------------------------------------
data Message
  = LobbyEvent LobbyEvent
  | RoomEvent RoomEvent


--------------------------------------------------------------------------------
data LobbyEvent
  = LobbyClientJoin Client
  | LobbyClientLeave Client
  | LobbyRoomMake Text Text Client
  | LobbyRoomJoin Text Text Client
  | LobbyRoomUpdate Room
  | LobbyRoomRemove Room


--------------------------------------------------------------------------------
data RoomEvent
  = RoomPlayerJoin Text Client
  | RoomPlayerLeave Client Bool -- dont use bool
  | RoomPlayerDisconnect Client


--------------------------------------------------------------------------------
data ClientEvent
  = ClientInbound Message
  | ClientOutbound Text
  | ClientRoomJoin (TChan RoomEvent)
  | ClientRoomLeave
  | ClientDisconnect


--------------------------------------------------------------------------------
type EventHandler msg state =
  state -> msg -> IO (Continuation state)


--------------------------------------------------------------------------------
data Continuation a
  = Result a
  | Skip
  | End


--------------------------------------------------------------------------------
instance Eq Client where
  Client { clientId = firstId } == Client { clientId = secondId } =
    firstId == secondId


--------------------------------------------------------------------------------
instance Ord Client where
  compare Client { clientId = firstId } Client { clientId = secondId } =
    compare firstId secondId


--------------------------------------------------------------------------------
main :: IO ()
main = do
  lobbyChannel <- STM.atomically STM.newTChan

  let initLobby = Lobby
        { lobbyChannel
        , lobbyClients   = Map.empty
        , lobbyRoomViews = Map.empty
        }

  Async.async $ processLobbyChannel lobbyChannel initLobby
  Warp.run 3000 $ app lobbyChannel


--------------------------------------------------------------------------------
app :: TChan LobbyEvent -> Application
app lobbyChannel =
  websocketsOr WS.defaultConnectionOptions (wsApp lobbyChannel) reqApp


--------------------------------------------------------------------------------
reqApp :: Application
reqApp _ respond = respond $ Wai.responseLBS HTTP.status400 [] "PLACEHOLDER"


--------------------------------------------------------------------------------
wsApp :: TChan LobbyEvent -> ServerApp
wsApp lobbyChannel pendingConnection = do
  connection <- WS.acceptRequest pendingConnection
  WS.withPingThread connection 30 (pure ()) $ do
    -- authResponse <- WS.receiveData connection

    clientChannel <- STM.atomically STM.newTChan

    let client = Client
          { clientChannel     = clientChannel
          , clientConnection  = connection
          , clientId          = "PLACEHOLDER ID"
          , clientRoomChannel = Nothing
          }

    Async.async $ processClientChannel lobbyChannel clientChannel client
    Async.async $ onMessage clientChannel connection >> onDisconnect clientChannel

    tellChannelIO lobbyChannel $ LobbyClientJoin client
    where
      onMessage :: TChan ClientEvent -> Connection -> IO ()
      onMessage channel = forever .
        tellChannelIO channel . ClientInbound <=< undefined -- WS.receiveData

      onDisconnect :: TChan ClientEvent -> IO ()
      onDisconnect =
        flip tellChannelIO ClientDisconnect


-------------------------------------------------------------------------------
processLobbyChannel :: TChan LobbyEvent -> Lobby -> IO ()
processLobbyChannel = processChannel lobbyEventHandler


-------------------------------------------------------------------------------
lobbyEventHandler :: EventHandler LobbyEvent Lobby
lobbyEventHandler lobby@Lobby { lobbyChannel, lobbyClients, lobbyRoomViews } = \case
  LobbyClientJoin Client { clientChannel, clientId } -> do
    tellChannelIO clientChannel $ ClientOutbound "room list placeholder"
    pure $ Result lobby { lobbyClients = Map.insert clientId clientChannel lobbyClients }

  LobbyClientLeave Client { clientId } ->
    pure $ Result lobby { lobbyClients = Map.delete clientId lobbyClients }

  LobbyRoomMake roomName playerName roomOwner -> do
    roomChannel <- STM.atomically STM.newTChan

    let
      room = Room
        { roomChannel
        , roomName
        , roomOwner
        , roomPlayers = Map.empty
        , roomPlaying = Nothing
        }

      roomView =
        roomToView room

    Async.async $ processRoomChannel lobbyChannel roomChannel room
    tellChannelIO roomChannel $ RoomPlayerJoin playerName roomOwner
    pure $ Result lobby { lobbyRoomViews = Map.insert roomName roomView lobbyRoomViews }

  LobbyRoomJoin roomName playerName client@Client { clientChannel } ->
    const (pure Skip) $
      case Map.lookup roomName lobbyRoomViews of
        Just RoomView { roomViewChannel } ->
          tellChannelIO roomViewChannel $ RoomPlayerJoin playerName client

        _ ->
          tellChannelIO clientChannel $ ClientOutbound "error bad placeholder"

  LobbyRoomUpdate room@Room { roomName } -> do
    tellClientsIO $ ClientOutbound "room update placeholder"
    pure $ Result lobby { lobbyRoomViews = Map.insert roomName (roomToView room) lobbyRoomViews }

  LobbyRoomRemove Room { roomName } -> do
    tellClientsIO $ ClientOutbound "room remove placeholder"
    pure $ Result lobby { lobbyRoomViews = Map.delete roomName lobbyRoomViews }
  where
    tellClientsIO :: ClientEvent -> IO ()
    tellClientsIO = STM.atomically . broadcastChannels lobbyClients


--------------------------------------------------------------------------------
roomToView :: Room -> RoomView
roomToView Room { roomChannel, roomName, roomPlayers, roomPlaying } =
  RoomView
    { roomViewChannel   = roomChannel
    , roomViewInGame    = Maybe.isJust roomPlaying
    , roomViewName      = roomName
    , roomViewOccupancy = length roomPlayers
    }


--------------------------------------------------------------------------------
processRoomChannel :: TChan LobbyEvent -> TChan RoomEvent -> Room -> IO ()
processRoomChannel = processChannel . roomEventHandler


--------------------------------------------------------------------------------
roomEventHandler :: TChan LobbyEvent -> EventHandler RoomEvent Room
roomEventHandler lobbyChannel room@Room { roomChannel, roomPlayers } = \case
  RoomPlayerJoin playerName client@Client { clientChannel } ->
    if | roomInGame room -> do
          tellChannelIO clientChannel $ ClientOutbound "in game error placeholder"
          pure Skip

       | playerInRoom playerName -> do
          tellChannelIO clientChannel $ ClientOutbound "player name taken error placeholder"
          pure Skip

       | otherwise -> do
          let
            newPlayer = Player
              { playerName
              , playerScore = 0
              }

            newRoom = room
              { roomPlayers = Map.insert client newPlayer roomPlayers
              }

          STM.atomically $ STM.writeTChan lobbyChannel (LobbyClientLeave client)
                         >> STM.writeTChan lobbyChannel (LobbyRoomUpdate newRoom)
                         >> STM.writeTChan clientChannel (ClientRoomJoin roomChannel)
                         >> tellPlayers roomPlayers (ClientOutbound "player joined placeholder")

          pure $ Result newRoom

  RoomPlayerLeave client@Client { clientChannel } disconnected ->
    if clientInRoom client then do
      let
        newRoomPlayers =
          Map.delete client roomPlayers

        ( continuation, lobbyEvent, playerAction ) =
          if null newRoomPlayers then
            ( End
            , LobbyRoomRemove room
            , pure ()
            )
          else
            let
              newRoom = room
                { roomPlayers = newRoomPlayers
                }
            in
              ( Result newRoom
              , LobbyRoomUpdate newRoom
              , tellPlayers newRoomPlayers (ClientOutbound "player left placeholder")
              )

        clientAction =
          if disconnected then
            pure ()
          else
            STM.writeTChan clientChannel ClientRoomLeave

      STM.atomically $ STM.writeTChan lobbyChannel lobbyEvent
                     >> STM.writeTChan lobbyChannel (LobbyClientJoin client)
                     >> clientAction
                     >> playerAction

      pure continuation
    else if disconnected then
      pure Skip
    else do
      tellChannelIO clientChannel $ ClientOutbound "client not in room error placeholder"
      pure Skip
  where
    tellPlayers :: Map Client v -> ClientEvent -> STM ()
    tellPlayers players =
      broadcastChannels (clientChannel <$> Map.keys players)

    clientInRoom :: Client -> Bool
    clientInRoom = flip Map.member roomPlayers

    playerInRoom :: Text -> Bool
    playerInRoom name =
      List.any ((name ==) . playerName) $ Map.elems roomPlayers

    roomInGame :: Room -> Bool
    roomInGame = Maybe.isJust . roomPlaying


-------------------------------------------------------------------------------
processClientChannel :: TChan LobbyEvent -> TChan ClientEvent -> Client -> IO ()
processClientChannel = processChannel . clientEventHandler


--------------------------------------------------------------------------------
clientEventHandler :: TChan LobbyEvent -> EventHandler ClientEvent Client
clientEventHandler
  lobbyChannel
  client@Client
    { clientConnection
    , clientRoomChannel
    }
  clientEvent =
    case clientEvent of
      ClientRoomJoin roomChannel -> do
        sendClient "you joined room placeholder"
        pure $ Result client { clientRoomChannel = Just roomChannel }

      ClientRoomLeave -> do
        sendClient "you left room placeholder"
        pure $ Result client { clientRoomChannel = Nothing }

      ClientInbound message -> const (pure Skip) $
        case ( message, clientRoomChannel ) of
          ( LobbyEvent lobbyEvent, Nothing ) ->
            tellLobbyIO lobbyEvent

          ( RoomEvent roomEvent, Just roomChannel ) ->
            tellChannelIO roomChannel roomEvent

          _ ->
            sendClient "error placeholder"

      ClientOutbound message -> do
        sendClient message
        pure Skip

      ClientDisconnect -> const (pure End) $
        case clientRoomChannel of
          Just roomChannel ->
            tellChannelIO roomChannel $ RoomPlayerLeave client True -- dont use bool

          _ ->
            tellChannelIO lobbyChannel $ LobbyClientLeave client
  where
    sendClient :: Text -> IO ()
    sendClient = WS.sendTextData clientConnection

    tellLobbyIO :: LobbyEvent -> IO ()
    tellLobbyIO = tellChannelIO lobbyChannel


--------------------------------------------------------------------------------
broadcastChannels :: Foldable t => t (TChan a) -> a -> STM ()
broadcastChannels channels event =
  foldl (\ts c -> ts >> STM.writeTChan c event) (pure ()) channels


--------------------------------------------------------------------------------
tellChannelIO :: TChan a -> a -> IO ()
tellChannelIO channel = STM.atomically . STM.writeTChan channel


--------------------------------------------------------------------------------
processChannel
  :: forall msg state
  .  EventHandler msg state
  -> TChan msg
  -> state
  -> IO ()
processChannel messageHandler channel = loop
  where
    loop :: state -> IO ()
    loop state =
      STM.atomically (STM.readTChan channel)
      >>= messageHandler state
      >>= \case
        Result newState ->
          loop newState

        Skip ->
          loop state

        End ->
          pure ()
