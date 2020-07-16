module Scrabble.Handler
  ( clientHandler
  , lobbyHandler
  , processQueue
  , roomHandler
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM   (TBQueue)
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
lobbyHandler context@Context { contextLobbyQueue = lobbyQueue } =
  createHandler handlerInternal handlerExternal
  where
    handlerInternal :: HandlerInternal Lobby LobbyEventInternal
    handlerInternal lobby = \case
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

      LobbyRoomRun room@Room { roomOwner } roomQueue ->
        let
          handler =
            roomHandler context

          runRoom =
            processQueue handler roomQueue room

          tellRoom =
            Event.emitIO roomQueue $ RoomPlayerJoin roomOwner roomQueue
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
    handlerExternal lobby client = \case
      LobbyRoomMake (RM { rmRoomCapacity = capacity, rmRoomName = roomName }) ->
        case Lobby.getRoom roomName lobby of
          Nothing ->
            if capacity > 0 && capacity <= Room.maxCapacity then
              let
                room = Room.new roomName capacity client
              in
                Right ( lobby
                      , STM.atomically
                          $ Event.emit lobbyQueue . LobbyRoomRun room
                          =<< STM.newTBQueue 256 -- todo
                      )
            else
              Left RoomCapacityInvalid

          _ ->
            Left RoomAlreadyExists

      LobbyRoomJoin (RJ { rjRoomName = roomName }) ->
        case Lobby.getRoom roomName lobby of
          Just ( RoomView { roomViewCapacity, roomViewInGame, roomViewOccupancy }, roomQueue ) ->
            if | roomViewInGame -> -- todo: refactor into functions consuming roomview
                  Left RoomInGame

               | roomViewOccupancy >= roomViewCapacity ->
                  Left RoomIsFull

               | otherwise ->
                  Right ( lobby
                        , Event.emitIO roomQueue $ RoomPlayerJoin client roomQueue
                        )

          _ ->
            Left RoomDoesNotExist


--------------------------------------------------------------------------------
roomHandler :: Context -> Handler Room RoomEvent
roomHandler Context { contextLobbyQueue = lobbyQueue } =
  createHandler handlerInternal handlerExternal
  where
    handlerInternal :: HandlerInternal Room RoomEventInternal
    handlerInternal room = \case
      RoomPlayerJoin client@Client { clientQueue } roomQueue ->
        let
          newRoom = Room.addPendingClient client room
        in
          ( newRoom
          , STM.atomically $ do
              Event.emit lobbyQueue $ LobbyClientLeave client
              Event.emit lobbyQueue $ LobbyRoomUpdate newRoom
              Event.emit clientQueue $ ClientRoomJoin newRoom roomQueue
          )

      RoomPlayerLeave client@Client { clientQueue } ->
        let ( removeClient, roomEvent ) =
              case Room.getPlayer client room of
                Just Player { playerName } ->
                  ( Room.removePlayer, PlayerLeaveOutbound playerName )

                _ ->
                  ( Room.removePendingClient, PendingClientLeaveOutbound )

            newRoom =
              removeClient client room

            ( lobbyEvent, roomAction ) =
              if Room.isEmpty newRoom then
                ( LobbyRoomRemove
                , exitSuccess
                )
              else
                ( LobbyRoomUpdate
                , broadcastRoom newRoom roomEvent
                )
        in
          ( newRoom
          , do
              STM.atomically $ do
                Event.emit lobbyQueue $ lobbyEvent newRoom
                Event.emit lobbyQueue $ LobbyClientJoin client
                Event.emit clientQueue ClientRoomLeave
              roomAction -- if broadcast then perform atomically ?
          )

    handlerExternal :: HandlerExternal Room RoomEventExternal Error
    handlerExternal room client = \case
      RoomPlayerSetName (PSN { psnPlayerName = playerName }) ->
        if Room.hasPlayerName playerName room then
          Left RoomHasPlayer
        else
          case Room.registerPendingClient client playerName room of
            Just newRoom ->
              Right ( newRoom
                    , broadcastRoom room $ PlayerJoinOutbound playerName
                    )

            _ ->
              Left PlayerNameAlreadySet


--------------------------------------------------------------------------------
clientHandler :: Context -> Handler ( Client, Maybe RoomQueue ) ClientEvent
clientHandler Context { contextLobbyQueue = lobbyQueue } =
  createHandler handlerInternal handlerExternal
  where
    handlerInternal :: HandlerInternal ( Client, Maybe RoomQueue ) ClientEventInternal
    handlerInternal state@( client@Client { clientConnection }, maybeRoomQueue ) = \case
      ClientMessageSend message ->
        ( state
        , WS.sendTextData clientConnection $ JSON.encode message
        )

      ClientRoomJoin room roomQueue ->
        ( ( client, Just roomQueue )
        , toClient client $ RoomJoinOutbound room
        )

      ClientRoomLeave ->
        ( ( client, Nothing )
        , toClient client RoomLeaveOutbound
        )

      ClientDisconnect ->
        ( state
        , let
            toEmit =
              case maybeRoomQueue of
                Just roomQueue ->
                  Event.emitIO roomQueue . RoomPlayerLeave

                _ ->
                  Event.emitIO lobbyQueue . LobbyClientLeave
          in
            toEmit client >> exitSuccess
        )

    handlerExternal :: HandlerExternal ( Client, Maybe RoomQueue ) ClientEventExternal Error
    handlerExternal state@( client, maybeRoomQueue ) _ = \case
      ClientMessageReceive received ->
        case received of
          Left decodeError ->
            Left $ MessageInvalid decodeError

          Right message ->
            ( state, ) <$>
              case maybeRoomQueue of
                Just roomQueue ->
                  whenInRoom roomQueue client message

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

    whenInRoom :: RoomQueue -> Client -> MessageInbound -> Either Error (IO ())
    whenInRoom roomQueue client = \case
      RoomLeaveInbound ->
        Right $ Event.emitIO roomQueue (RoomPlayerLeave client)

      PlayerSetNameInbound playerSetName ->
        Right $ Event.passIO roomQueue client (RoomPlayerSetName playerSetName)

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
