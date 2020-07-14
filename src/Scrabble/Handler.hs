module Scrabble.Handler
  ( clientHandler
  , lobbyHandler
  , processQueue
  , roomHandler
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM   (STM, TBQueue)
import           Data.Text                (Text)
import           System.Exit              (exitSuccess)

import           Scrabble.Message         (Message (..))
import           Scrabble.Types

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM   as STM

import qualified Scrabble.Lobby           as Lobby
import qualified Scrabble.Player          as Player
import qualified Scrabble.Room            as Room


--------------------------------------------------------------------------------
type Handler state event =
  state -> event -> ( state, IO () )


--------------------------------------------------------------------------------
type HandlerInternal state event =
  state -> event -> ( state, IO () )


--------------------------------------------------------------------------------
type HandlerExternal state event error =
  state -> Client -> event -> Either error ( state, IO () )


--------------------------------------------------------------------------------
lobbyHandler :: Handler Lobby LobbyEvent
lobbyHandler =
  createHandler handlerInternal handlerExternal
  where
    handlerInternal :: HandlerInternal Lobby LobbyEventInternal
    handlerInternal lobby@Lobby { lobbyQueue } = \case
      LobbyClientJoin client ->
        let
          roomViews = Lobby.listRoomViews lobby
        in
          ( Lobby.addClient client lobby
          , toClient client $ RoomListOutbound roomViews
          )

      LobbyClientLeave client ->
        ( Lobby.removeClient client lobby
        , noAction
        )

      LobbyRoomRun room@Room { roomOwner } ownerName roomQueue ->
        let
          handler =
            roomHandler lobbyQueue

          runRoom =
            processQueue handler roomQueue room

          tellRoom =
            emitEventIO roomQueue $ RoomOwnerJoin roomOwner ownerName
        in
          ( Lobby.addRoom roomQueue room lobby
          , Async.async runRoom >> tellRoom
          )

      LobbyRoomUpdate room ->
        let
          roomView = Room.toView room
        in
          ( Lobby.updateRoomView roomView lobby
          , broadcastLobby lobby $ RoomUpdateOutbound roomView
          )

      LobbyRoomRemove room@Room { roomName } ->
        ( Lobby.removeRoom room lobby
        , broadcastLobby lobby $ RoomRemoveOutbound roomName
        )

    handlerExternal :: HandlerExternal Lobby LobbyEventExternal Error
    handlerExternal lobby@Lobby { lobbyQueue } client = \case
      LobbyRoomMake (RM { rmRoomCapacity = capacity, rmPlayerName = playerName, rmRoomName = roomName }) ->
        case Lobby.getRoom roomName lobby of
          Nothing ->
            if capacity > 0 && capacity <= Room.maxCapacity then
              let
                roomOwner = Player.new playerName
                room = Room.new roomName capacity roomOwner
              in
                Right ( lobby
                      , STM.atomically
                          $ emitEvent lobbyQueue . LobbyRoomRun room playerName
                          =<< STM.newTBQueue 256 -- todo
                      )
            else
              Left RoomCapacityInvalid

          _ ->
            Left RoomAlreadyExists

      LobbyRoomJoin (roomJoin@RJ { rjRoomName = roomName }) ->
        case Lobby.getRoom roomName lobby of
          Just ( _, roomQueue ) ->
            Right ( lobby
                  , passEventIO roomQueue client $ RoomPlayerJoin roomJoin
                  )

          _ ->
            Left RoomDoesNotExist


--------------------------------------------------------------------------------
roomHandler :: LobbyQueue -> Handler Room RoomEvent
roomHandler lobbyQueue =
  createHandler handlerInternal handlerExternal
  where
    handlerInternal :: HandlerInternal Room RoomEventInternal
    handlerInternal room = \case
      RoomOwnerJoin client ownerName ->
        addPlayer client ownerName room

      RoomPlayerLeave client playerName ->
        removePlayer client playerName room

    handlerExternal :: HandlerExternal Room RoomEventExternal Error
    handlerExternal room client = \case
      RoomPlayerJoin (RJ { rjPlayerName = playerName }) ->
        if | Room.inGame room ->
              Left RoomInGame

           | Room.isFull room ->
              Left RoomIsFull

           | Room.hasPlayerName playerName room ->
              Left RoomHasPlayer

           | otherwise ->
              Right $ addPlayer client playerName room


    addPlayer :: Client -> Text -> Room -> ( Room, IO () )
    addPlayer client@Client { clientQueue } name room =
      let
        newRoom = Room.addPlayer client name room
      in
        ( newRoom
        , STM.atomically $ do
            emitEvent lobbyQueue $ LobbyClientLeave client
            emitEvent lobbyQueue $ LobbyRoomUpdate newRoom
            emitEvent clientQueue $ ClientRoomJoin newRoom
        )

    removePlayer :: Client -> Text -> Room -> ( Room, IO () )
    removePlayer client@Client { clientQueue } name room =
      let ( newState, lobbyEvent, roomAction ) =
            case Room.removePlayer client room of
              Just newRoom ->
                ( newRoom
                , LobbyRoomUpdate
                , broadcastRoom newRoom $ PlayerLeaveOutbound name
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
              emitEvent lobbyQueue $ lobbyEvent newState
              emitEvent lobbyQueue $ LobbyClientJoin client
              emitEvent clientQueue ClientRoomLeave
            roomAction
        )


--------------------------------------------------------------------------------
clientHandler :: LobbyQueue -> Handler ( Client, Maybe RoomHandle ) ClientEvent
clientHandler lobbyQueue =
  createHandler handlerInternal handlerExternal
  where
    handlerInternal :: HandlerInternal ( Client, Maybe RoomHandle ) ClientEventInternal
    handlerInternal state@( client, maybeHandle ) = \case
      ClientRoomJoin roomQueue ->
        ( ( client, undefined {- Just roomQueue -} )
        , toClient client $ RoomJoinOutbound roomQueue
        )

      ClientRoomLeave ->
        ( ( client, Nothing )
        , toClient client RoomLeaveOutbound
        )

      ClientDisconnect ->
        ( state
        , let
            toEmit =
              case maybeHandle of
                Just RoomHandle { roomHandleQueue, roomHandlePlayerName } ->
                  emitEventIO roomHandleQueue
                  . flip RoomPlayerLeave
                    roomHandlePlayerName

                _ ->
                  emitEventIO lobbyQueue
                  . LobbyClientLeave
          in
            toEmit client >> exitSuccess
        )

    handlerExternal :: HandlerExternal ( Client, Maybe RoomHandle ) ClientEventExternal Error
    handlerExternal state@( client, maybeHandle ) _ = \case
      ClientMessageSend received ->
        case received of
          Left decodeError ->
            Left $ MessageInvalid decodeError

          Right message ->
            ( state, ) <$>
              case maybeHandle of
                Just RoomHandle { roomHandlePlayerName, roomHandleQueue } ->
                  emitEventIO roomHandleQueue <$> whenInRoom client roomHandlePlayerName message

                _ ->
                  passEventIO lobbyQueue client <$> whenInLobby message

    whenInLobby :: MessageInbound -> Either Error LobbyEventExternal
    whenInLobby = \case
      RoomMakeInbound roomMake ->
        Right $ LobbyRoomMake roomMake

      RoomJoinInbound roomJoin ->
        Right $ LobbyRoomJoin roomJoin

      _ ->
        Left ClientNotInLobby

    whenInRoom :: Client -> Text -> MessageInbound -> Either Error RoomEventInternal
    whenInRoom client playerName = \case
      RoomLeaveInbound ->
        Right $ RoomPlayerLeave client playerName

      _ ->
        Left ClientNotInRoom


--------------------------------------------------------------------------------
createHandler
  :: HandlerInternal state internal
  -> HandlerExternal state external Error
  -> Handler state (Event internal external)
createHandler
  handlerInternal
  handlerExternal
  state = \case
    EventInternal event ->
      handlerInternal state event

    EventExternal client event ->
      case handlerExternal state client event of
        Right result ->
          result

        Left err ->
          ( state
          , toClient client $ ErrorOutbound err
          )


--------------------------------------------------------------------------------
passEventIO :: TBQueue (Event i e) -> Client -> e -> IO ()
passEventIO queue client = STM.atomically . passEvent queue client


--------------------------------------------------------------------------------
passEvent :: TBQueue (Event i e) -> Client -> e -> STM ()
passEvent queue client = STM.writeTBQueue queue . EventExternal client


--------------------------------------------------------------------------------
emitEventIO :: TBQueue (Event i e) -> i -> IO ()
emitEventIO queue = STM.atomically . emitEvent queue


--------------------------------------------------------------------------------
emitEvent :: TBQueue (Event i e) -> i -> STM ()
emitEvent queue = STM.writeTBQueue queue . EventInternal


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

      action
      loop newState
