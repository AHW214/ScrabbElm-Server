module Scrabble.Handler
  ( clientHandler
  , lobbyHandler
  , processQueue
  , roomHandler
  ) where


--------------------------------------------------------------------------------
import           System.Exit              (exitSuccess)

import           Scrabble.Types

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM   as STM
import qualified Data.Aeson               as JSON
import qualified Network.WebSockets       as WS

import qualified Scrabble.Lobby           as Lobby
import qualified Scrabble.Room            as Room


--------------------------------------------------------------------------------
type Handler state event =
  state -> event -> ( state, IO () )


--------------------------------------------------------------------------------
lobbyHandler :: Context -> Handler Lobby (Event Lobby)
lobbyHandler context@Context { contextLobbyQueue = lobbyQueue } lobby = \case
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
        emitIO roomQueue $ RoomPlayerJoin roomOwner roomQueue
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

  LobbyRoomMake (RM { rmRoomCapacity = capacity, rmRoomName = roomName }) client ->
    ( lobby
    , case Lobby.getRoom roomName lobby of
        Nothing ->
          if capacity > 0 && capacity <= Room.maxCapacity then
            let
              room = Room.new roomName capacity client
            in
              STM.atomically
                $ emit lobbyQueue . LobbyRoomRun room
                =<< STM.newTBQueue 256 -- todo
          else
            errorToClient client RoomCapacityInvalid

        _ ->
          errorToClient client RoomAlreadyExists
    )

  LobbyRoomJoin (RJ { rjRoomName = roomName }) client ->
    ( lobby
    , case Lobby.getRoom roomName lobby of
        Just ( RoomView { roomViewCapacity, roomViewInGame, roomViewOccupancy }, roomQueue ) ->
          if | roomViewInGame -> -- todo: refactor into functions consuming roomview
                errorToClient client RoomInGame

             | roomViewOccupancy >= roomViewCapacity ->
                errorToClient client RoomIsFull

             | otherwise ->
                emitIO roomQueue $ RoomPlayerJoin client roomQueue

        _ ->
          errorToClient client RoomDoesNotExist
    )


--------------------------------------------------------------------------------
roomHandler :: Context -> Handler Room (Event Room)
roomHandler Context { contextLobbyQueue = lobbyQueue } room = \case
  RoomPlayerJoin client@Client { clientQueue } roomQueue ->
    let
      newRoom = Room.addPendingClient client room
    in
      ( newRoom
      , STM.atomically $ do
          emit lobbyQueue $ LobbyClientLeave client
          emit lobbyQueue $ LobbyRoomUpdate newRoom
          emit clientQueue $ ClientRoomJoin newRoom roomQueue
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
            emit lobbyQueue $ lobbyEvent newRoom
            emit lobbyQueue $ LobbyClientJoin client
            emit clientQueue ClientRoomLeave
          roomAction -- if broadcast then perform atomically ?
      )

  RoomPlayerSetName (PSN { psnPlayerName = playerName }) client ->
    if Room.hasPlayerName playerName room then
      ( room
      , errorToClient client RoomHasPlayer
      )
    else
      case Room.registerPendingClient client playerName room of
        Just newRoom ->
          ( newRoom
          , broadcastRoom room $ PlayerJoinOutbound playerName
          )

        _ ->
          ( room
          , errorToClient client PlayerNameAlreadySet
          )


--------------------------------------------------------------------------------
clientHandler :: Context -> Handler ( Client, Maybe (EventQueue Room) ) (Event Client)
clientHandler Context { contextLobbyQueue = lobbyQueue } state@( client@Client { clientConnection }, maybeRoomQueue ) = \case
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
              emitIO roomQueue . RoomPlayerLeave

            _ ->
              emitIO lobbyQueue . LobbyClientLeave
      in
        toEmit client >> exitSuccess
    )

  ClientMessageReceive received ->
    ( state
    , case received of
        Left decodeError ->
          errorToClient client $ MessageInvalid decodeError

        Right message ->
          let
            withClient =
              case maybeRoomQueue of
                Just roomQueue ->
                  whenInRoom roomQueue message

                _ ->
                  whenInLobby message
          in
            withClient client
    )
  where
    whenInLobby :: Message Inbound -> Client -> IO ()
    whenInLobby = \case
      RoomMakeInbound roomMake ->
        emitIO lobbyQueue . LobbyRoomMake roomMake

      RoomJoinInbound roomJoin ->
        emitIO lobbyQueue . LobbyRoomJoin roomJoin

      _ ->
        flip errorToClient ClientNotInLobby

    whenInRoom :: EventQueue Room -> Message Inbound -> Client -> IO ()
    whenInRoom roomQueue = \case
      RoomLeaveInbound ->
        emitIO roomQueue . RoomPlayerLeave

      PlayerSetNameInbound playerSetName ->
        emitIO roomQueue . RoomPlayerSetName playerSetName

      _ ->
        flip errorToClient ClientNotInRoom


--------------------------------------------------------------------------------
noAction :: IO ()
noAction = pure ()


--------------------------------------------------------------------------------
processQueue
  :: forall a
   . Model a
  => Handler a (Event a)
  -> EventQueue a
  -> a
  -> IO ()
processQueue handler queue = loop
  where
    loop :: a -> IO ()
    loop model = do
      event <- receiveIO queue
      let ( newModel, action ) = handler model event

      action
      loop newModel
