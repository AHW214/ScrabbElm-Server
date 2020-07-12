module Scrabble.Handler
  ( processQueue
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM   (STM, TBQueue)
import           TextShow                 (FromStringShow (..), TextShow (..))
import           System.Exit              (exitSuccess)

import           Scrabble.Client          (Client (..), ClientEvent (..))
import           Scrabble.Lobby           (Lobby (..), LobbyEvent (..))
import           Scrabble.Player          (Player (..))
import           Scrabble.Room            (Room (..), RoomView (..),
                                           RoomEvent (..))

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM   as STM
import qualified Data.Map                 as Map

import qualified Scrabble.Client          as Client
import qualified Scrabble.Lobby           as Lobby
import qualified Scrabble.Player          as Player
import qualified Scrabble.Room            as Room


--------------------------------------------------------------------------------
type Handler state event =
  state -> event -> ( state, IO () )


--------------------------------------------------------------------------------
data Error
  = ClientNotInRoom
  | RoomCapacityInvalid
  | RoomAlreadyExists
  | RoomDoesNotExist
  | RoomIsFull
  | RoomInGame
  | RoomHasPlayer
  deriving Show


--------------------------------------------------------------------------------
instance TextShow Error where
  showbPrec p = showbPrec p . FromStringShow


--------------------------------------------------------------------------------
class Emit receiver emission where
  emitEvent :: receiver -> emission -> STM ()

  emitEventIO :: receiver -> emission -> IO ()
  emitEventIO receiver =
    STM.atomically . emitEvent receiver


--------------------------------------------------------------------------------
instance Emit Lobby LobbyEvent where
  emitEvent Lobby { lobbyQueue } =
    STM.writeTBQueue lobbyQueue


--------------------------------------------------------------------------------
instance Emit Room RoomEvent where
  emitEvent Room { roomQueue } =
    STM.writeTBQueue roomQueue


--------------------------------------------------------------------------------
instance Emit RoomView RoomEvent where
  emitEvent RoomView { roomViewQueue } =
    STM.writeTBQueue roomViewQueue


--------------------------------------------------------------------------------
instance Emit Client ClientEvent where
  emitEvent Client { clientQueue } =
    STM.writeTBQueue clientQueue


--------------------------------------------------------------------------------
instance Emit (TBQueue a) a where
  emitEvent = STM.writeTBQueue


--------------------------------------------------------------------------------
lobbyHandler :: Handler Lobby LobbyEvent
lobbyHandler lobby@Lobby { lobbyQueue } = \case
  LobbyClientJoin client ->
    ( Lobby.addClient client lobby
    , tellClientIO client ("room list placeholder" :: String)
    )

  LobbyClientLeave client ->
    ( Lobby.removeClient client lobby
    , noAction
    )

  LobbyRoomMake roomName roomCapacity playerName client ->
    ( lobby
    , case Lobby.getRoomView roomName lobby of
        Nothing ->
          if roomCapacity > 0 && roomCapacity <= Room.maxCapacity then
            let
              roomOwner = Player.new playerName client
              toRoom = Room.new roomName roomCapacity roomOwner
            in
              STM.atomically
                $ STM.writeTBQueue lobbyQueue . LobbyRoomRun . toRoom
                =<< STM.newTBQueue 256 -- todo
          else
            tellClientErrorIO client RoomCapacityInvalid

        _ ->
          tellClientErrorIO client RoomAlreadyExists
    )

  LobbyRoomRun room@Room { roomOwner, roomQueue } ->
    let
      Player { playerName, playerClient } =
        roomOwner

      handler =
        roomHandler lobby

      runRoom =
        processQueue handler roomQueue room

      tellRoom =
        emitEventIO room $ RoomPlayerJoin playerName playerClient
    in
      ( Lobby.addRoomView room lobby
      , Async.async runRoom >> tellRoom
      )

  LobbyRoomJoin roomName playerName client ->
    ( lobby
    , case Lobby.getRoomView roomName lobby of
        Just roomView ->
          emitEventIO roomView $ RoomPlayerJoin playerName client

        _ ->
          tellClientErrorIO client RoomDoesNotExist
    )

  LobbyRoomUpdate room ->
    ( Lobby.updateRoomView room lobby
    , broadcastLobbyIO lobby ("room update placeholder" :: String)
    )

  LobbyRoomRemove room ->
    ( Lobby.removeRoomView room lobby
    , broadcastLobbyIO lobby ("room remove placeholder" :: String)
    )


--------------------------------------------------------------------------------
roomHandler :: Lobby -> Handler Room RoomEvent
roomHandler lobby room = \case
  RoomPlayerJoin playerName client ->
    if | Room.inGame room ->
          ( room
          , tellClientErrorIO client RoomInGame
          )

       | Room.isFull room ->
          ( room
          , tellClientErrorIO client RoomIsFull
          )

       | Room.hasPlayerTag playerName room ->
          ( room
          , tellClientErrorIO client RoomHasPlayer
          )

       | otherwise ->
          let
            newPlayer = Player.new playerName client
            newRoom = Room.addPlayer newPlayer room
          in
            ( newRoom
            , STM.atomically $ do
                emitEvent lobby (LobbyClientLeave client)
                emitEvent lobby (LobbyRoomUpdate newRoom) -- todo: more events
            )

  RoomPlayerLeave client ->
    case Room.getPlayer client room of
      Just player ->
        let ( newState, lobbyEvent, roomAction ) =
              case Room.removePlayer player room of
                Just newRoom ->
                  ( newRoom
                  , LobbyRoomUpdate
                  , broadcastRoomIO newRoom ("player left placeholder" :: String)
                  )

                _ ->
                  ( room
                  , LobbyRoomRemove
                  , exitSuccess
                  )
        in
          ( newState
          , do
              STM.atomically $ do
                emitEvent lobby $ lobbyEvent newState
                emitEvent lobby $ LobbyClientJoin client
                tellClient client ("you left room placeholder" :: String)
              roomAction -- try to place broadcast in atomic action ?
          )

      _ ->
        ( room
        , tellClientErrorIO client ClientNotInRoom
        )


--------------------------------------------------------------------------------
clientHandler :: Lobby -> Handler Client ClientEvent
clientHandler lobby client = \case
  ClientInbound _ ->
    ( client
    , noAction
    )

  ClientOutbound message ->
    ( client
    , Client.send client message
    )

  ClientRoomJoin _ ->
    ( client
    , Client.send client "you joined room placeholder"
    )

  ClientRoomLeave ->
    ( Client.leaveRoom client
    , Client.send client "you left room placeholder"
    )

  ClientDisconnect ->
    ( client
    , let
        toEmit =
          case clientRoomQueue client of
            Just roomQueue ->
              emitEventIO roomQueue . RoomPlayerLeave

            _ ->
              emitEventIO lobby . LobbyClientLeave
      in
        toEmit client >> exitSuccess
    )


--------------------------------------------------------------------------------
broadcastLobbyIO :: TextShow a => Lobby -> a -> IO ()
broadcastLobbyIO lobby =
  STM.atomically . broadcastLobby lobby


--------------------------------------------------------------------------------
broadcastLobby :: TextShow a => Lobby -> a -> STM ()
broadcastLobby Lobby { lobbyClientQueues } =
  broadcastQueues lobbyClientQueues . ClientOutbound . showt


--------------------------------------------------------------------------------
broadcastRoomIO :: TextShow a => Room -> a -> IO ()
broadcastRoomIO room =
  STM.atomically . broadcastRoom room


--------------------------------------------------------------------------------
broadcastRoom :: TextShow a => Room -> a -> STM ()
broadcastRoom Room { roomPlayers } =
  broadcastQueues (clientQueue <$> Map.keys roomPlayers) . ClientOutbound . showt


--------------------------------------------------------------------------------
tellClientErrorIO :: Client -> Error -> IO ()
tellClientErrorIO client =
  STM.atomically . tellClientError client


--------------------------------------------------------------------------------
tellClientError :: Client -> Error -> STM ()
tellClientError = tellClient


--------------------------------------------------------------------------------
tellClientIO :: TextShow a => Client -> a -> IO ()
tellClientIO client =
  STM.atomically . tellClient client


--------------------------------------------------------------------------------
tellClient :: TextShow a => Client -> a -> STM ()
tellClient client =
  emitEvent client . ClientOutbound . showt


--------------------------------------------------------------------------------
broadcastQueues :: Foldable t => t (TBQueue a) -> a -> STM ()
broadcastQueues queues event =
  foldl (\ts q -> ts >> STM.writeTBQueue q event) (pure ()) queues


--------------------------------------------------------------------------------
noAction :: IO ()
noAction = pure ()


--------------------------------------------------------------------------------
processQueue
  :: forall state event
  .  Handler state event
  -> TBQueue event
  -> state
  -> IO ()
processQueue handler queue = loop
  where
    loop :: state -> IO ()
    loop state = do
      event <- STM.atomically $ STM.readTBQueue queue
      let ( newState, action ) = handler state event
      action >> loop newState
