module Scrabble.Handler
  ( clientHandler
  , gatewayHandler
  , lobbyHandler
  , processQueue
  , roomHandler
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.Async (Async)
import           Control.Concurrent.STM   (TBQueue)
import           Data.Text                (Text)
import           Data.Time                (NominalDiffTime)
import           System.Exit              (exitSuccess)

import           Scrabble.Types

import qualified Control.Concurrent       as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM   as STM
import qualified Data.Time                as Time

import qualified Scrabble.Client          as Client
import qualified Scrabble.Gateway         as Gateway
import qualified Scrabble.Lobby           as Lobby
import qualified Scrabble.Room            as Room


--------------------------------------------------------------------------------
type Handler model =
  model -> Event model -> ( model, IO () )


--------------------------------------------------------------------------------
gatewayHandler :: Context -> Handler Gateway
gatewayHandler
  context@Context
    { contextLobbyQueue = lobbyQueue
    }
  gateway@Gateway
    { gatewayQueue
    , gatewayTimeoutLength
    }
  = \case
    GatewayCreateJWT jwtQueue ->
      let
        ( newGateway, clientId ) =
          Gateway.createClientId gateway
      in
        ( newGateway
        , do
            jwt <- Gateway.createClientJWT clientId gateway
            timeout <- createTimeout clientId

            STM.atomically $ do
              STM.writeTBQueue jwtQueue jwt
              emit gatewayQueue $ GatewayAddTimeout clientId timeout
        )

    GatewayAddTimeout clientId timeout ->
      ( Gateway.addTimeout clientId timeout gateway
      , noAction
      )

    GatewayDeleteTimeout clientId ->
      ( Gateway.deleteTimeout clientId gateway
      , noAction
      )

    GatewayAuthenticate response connection wsQueue ->
      case Gateway.verifyClientJWT gateway response of
        Just clientId ->
          case Gateway.removeTimeout clientId gateway of
            Just ( newGateway, timeout ) ->
              ( newGateway
              , do
                  Async.cancel timeout

                  clientQueue <- newQueueIO

                  let
                    client = Client.new connection clientId clientQueue
                    handler = clientHandler context

                  Async.async $ processQueue handler clientQueue client

                  STM.atomically $ do
                    emit lobbyQueue $ LobbyClientJoin client
                    STM.writeTBQueue wsQueue $ Right clientQueue
              )

            _ ->
              ( gateway
              , errorToWS wsQueue AuthIdentityInvalid
              )

        _ ->
          ( gateway
          , errorToWS wsQueue AuthFormatInvalid
          )
  where
    createTimeout :: Text -> IO (Async ())
    createTimeout =
      Async.async
      . (Concurrent.threadDelay timeoutMicroseconds >>)
      . emitIO gatewayQueue
      . GatewayDeleteTimeout

    timeoutMicroseconds :: Int
    timeoutMicroseconds =
      nominalToMicroseconds gatewayTimeoutLength

    nominalToMicroseconds :: NominalDiffTime -> Int
    nominalToMicroseconds =
      floor . (1e6 *) . Time.nominalDiffTimeToSeconds

    errorToWS :: TBQueue (Either Error (EventQueue Client)) -> Error -> IO ()
    errorToWS wsQueue = STM.atomically . STM.writeTBQueue wsQueue . Left


--------------------------------------------------------------------------------
lobbyHandler :: Context -> Handler Lobby
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
                =<< newQueue
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
roomHandler :: Context -> Handler Room
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
clientHandler :: Context -> Handler Client
clientHandler Context { contextLobbyQueue = lobbyQueue } client@Client { clientConnection, clientRoomQueue } = \case
  ClientMessageSend message ->
    ( client
    , toConnection clientConnection message
    )

  ClientRoomJoin room roomQueue ->
    ( Client.joinRoom roomQueue client
    , toClient client $ RoomJoinOutbound room
    )

  ClientRoomLeave ->
    ( Client.leaveRoom client
    , toClient client RoomLeaveOutbound
    )

  ClientDisconnect ->
    ( client
    , let
        toEmit =
          case clientRoomQueue of
            Just roomQueue ->
              emitIO roomQueue . RoomPlayerLeave

            _ ->
              emitIO lobbyQueue . LobbyClientLeave
      in
        toEmit client >> exitSuccess
    )

  ClientMessageReceive received ->
    ( client
    , case received of
        Left decodeError ->
          errorToClient client $ MessageInvalid decodeError

        Right message ->
          let
            withClient =
              case clientRoomQueue of
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
processQueue :: forall a. Model a => Handler a -> EventQueue a -> a -> IO ()
processQueue handler queue = loop
  where
    loop :: a -> IO ()
    loop model = do
      event <- receiveIO queue
      let ( newModel, action ) = handler model event

      action
      loop newModel
