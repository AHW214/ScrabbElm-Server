module Scrabble.Handler
  ( clientHandler
  , lobbyHandler
  , processQueue
  , roomHandler
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM   (TBQueue)
import           Data.Text                (Text)
import           System.Exit              (exitSuccess)

import           Scrabble.Message         (Message (..))
import           Scrabble.Types

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM   as STM
import qualified Data.Aeson               as JSON
import qualified Network.WebSockets       as WS

import qualified Scrabble.Event           as Event
import qualified Scrabble.Lobby           as Lobby
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
lobbyHandler :: Context -> Handler Lobby LobbyEvent
lobbyHandler context = -- do something with context [e.g. replace lobbyQueue param]
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
            Event.emitIO roomQueue $ RoomOwnerJoin roomOwner ownerName
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
                room = Room.new roomName capacity client
              in
                Right ( lobby
                      , STM.atomically
                          $ Event.emit lobbyQueue . LobbyRoomRun room playerName
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
                  , Event.passIO roomQueue client $ RoomPlayerJoin roomJoin
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
            Event.emit lobbyQueue $ LobbyClientLeave client
            Event.emit lobbyQueue $ LobbyRoomUpdate newRoom
            Event.emit clientQueue $ ClientRoomJoin newRoom
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
              Event.emit lobbyQueue $ lobbyEvent newState
              Event.emit lobbyQueue $ LobbyClientJoin client
              Event.emit clientQueue ClientRoomLeave
            roomAction
        )


--------------------------------------------------------------------------------
clientHandler :: LobbyQueue -> Handler ( Client, Maybe RoomHandle ) ClientEvent
clientHandler lobbyQueue =
  createHandler handlerInternal handlerExternal
  where
    handlerInternal :: HandlerInternal ( Client, Maybe RoomHandle ) ClientEventInternal
    handlerInternal state@( client@Client { clientConnection }, maybeHandle ) = \case
      ClientMessageSend message ->
        ( state
        , WS.sendTextData clientConnection $ JSON.encode message
        )

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
                  Event.emitIO roomHandleQueue
                  . flip RoomPlayerLeave
                    roomHandlePlayerName

                _ ->
                  Event.emitIO lobbyQueue
                  . LobbyClientLeave
          in
            toEmit client >> exitSuccess
        )

    handlerExternal :: HandlerExternal ( Client, Maybe RoomHandle ) ClientEventExternal Error
    handlerExternal state@( client, maybeHandle ) _ = \case
      ClientMessageReceive received ->
        case received of
          Left decodeError ->
            Left $ MessageInvalid decodeError

          Right message ->
            ( state, ) <$>
              case maybeHandle of
                Just RoomHandle { roomHandlePlayerName, roomHandleQueue } ->
                  Event.emitIO roomHandleQueue <$> whenInRoom client roomHandlePlayerName message

                _ ->
                  Event.passIO lobbyQueue client <$> whenInLobby message

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
