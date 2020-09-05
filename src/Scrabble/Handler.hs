module Scrabble.Handler
  ( clientHandler,
    gatewayHandler,
    lobbyHandler,
    processQueue,
    roomHandler,
  )
where

--------------------------------------------------------------------------------

import qualified Control.Concurrent as Concurrent
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime)
import qualified Data.Time as Time
import qualified Scrabble.Client as Client
import qualified Scrabble.Gateway as Gateway
import qualified Scrabble.Lobby as Lobby
import Scrabble.Log (Logger (..))
import qualified Scrabble.Room as Room
import Scrabble.Types
import System.Exit (exitSuccess)
import TextShow (showt)

--------------------------------------------------------------------------------
type Handler model =
  model -> Event model -> (model, IO ())

--------------------------------------------------------------------------------
gatewayHandler :: Context -> Handler Gateway
gatewayHandler
  context@Context
    { contextLobbyQueue = lobbyQueue
    }
  gateway@Gateway
    { gatewayQueue,
      gatewayTimeoutLength
    } =
    \case
      GatewayCreateJWT jwtQueue ->
        let (newGateway, clientId) =
              Gateway.createClientId gateway
         in ( newGateway,
              do
                logInfo context $ "Sending token to client #" <> clientId

                jwt <- Gateway.createClientJWT clientId gateway
                timeout <- createTimeout clientId

                STM.atomically $ do
                  STM.writeTBQueue jwtQueue jwt
                  emit gatewayQueue $ GatewayAddTimeout clientId timeout
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
      GatewayAddTimeout clientId timeout ->
        ( Gateway.addTimeout clientId timeout gateway,
          logInfo context $ "Token sent to client #" <> clientId
        )
      GatewayDeleteTimeout clientId ->
        ( Gateway.deleteTimeout clientId gateway,
          logWarning context $ "Timing out client #" <> clientId
        )
      GatewayAuthenticate response connection wsQueue ->
        case Gateway.verifyClientJWT gateway response of
          Just clientId ->
            case Gateway.removeTimeout clientId gateway of
              Just (newGateway, timeout) ->
                ( newGateway,
                  do
                    Async.cancel timeout

                    logInfo context $ "Starting client process for client #" <> clientId

                    clientQueue <- newQueueIO

                    let client = Client.new connection clientId clientQueue
                        handler = clientHandler context

                    Async.async $ processQueue handler clientQueue client

                    STM.atomically $ do
                      emit lobbyQueue $ LobbyClientJoin client
                      STM.writeTBQueue wsQueue $ Right clientQueue
                )
              _ ->
                ( gateway,
                  errorToWS AuthIdentityInvalid
                )
          _ ->
            ( gateway,
              errorToWS AuthFormatInvalid
            )
        where
          errorToWS :: Error -> IO ()
          errorToWS err = do
            logWarning context $
              Text.unlines
                [ "Rejecting client",
                  "Reason: " <> showt err
                ]

            STM.atomically $ STM.writeTBQueue wsQueue (Left err)

--------------------------------------------------------------------------------
lobbyHandler :: Context -> Handler Lobby
lobbyHandler context@Context {contextLobbyQueue = lobbyQueue} lobby = \case
  LobbyClientJoin client ->
    let roomViews = Lobby.listRoomViews lobby
     in ( Lobby.addClient client lobby,
          toClient client $ OutboundRoomList roomViews
        )
  LobbyClientLeave client ->
    ( Lobby.removeClient client lobby,
      noAction
    )
  LobbyRoomRun room@Room {roomOwner} roomQueue ->
    let handler =
          roomHandler context

        runRoom =
          processQueue handler roomQueue room

        tellRoom =
          emitIO roomQueue $ RoomPlayerJoin roomOwner roomQueue
     in ( Lobby.addRoom roomQueue room lobby,
          Async.async runRoom >> tellRoom
        )
  LobbyRoomUpdate room ->
    let roomView = Room.toView room
     in ( Lobby.updateRoomView roomView lobby,
          broadcastLobby lobby $ OutboundRoomUpdate roomView
        )
  LobbyRoomRemove room@Room {roomName} ->
    ( Lobby.removeRoom room lobby,
      broadcastLobby lobby $ OutboundRoomRemove roomName
    )
  LobbyRoomMake (RM {rmRoomCapacity = capacity, rmRoomName = roomName}) client ->
    ( lobby,
      case Lobby.getRoom roomName lobby of
        Nothing ->
          if capacity > 0 && capacity <= Room.maxCapacity
            then
              let room = Room.new roomName capacity client
               in STM.atomically $
                    emit lobbyQueue . LobbyRoomRun room
                      =<< newQueue
            else errorToClient client RoomCapacityInvalid
        _ ->
          errorToClient client RoomAlreadyExists
    )
  LobbyRoomJoin (RJ {rjRoomName = roomName}) client ->
    ( lobby,
      case Lobby.getRoom roomName lobby of
        Just (RoomView {roomViewCapacity, roomViewInGame, roomViewOccupancy}, roomQueue) ->
          if
              | roomViewInGame -> -- todo: refactor into functions consuming roomview
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
roomHandler Context {contextLobbyQueue = lobbyQueue} room = \case
  RoomPlayerJoin client@Client {clientQueue} roomQueue ->
    let newRoom = Room.addPendingClient client room
     in ( newRoom,
          STM.atomically $ do
            emit lobbyQueue $ LobbyClientLeave client
            emit lobbyQueue $ LobbyRoomUpdate newRoom
            emit clientQueue $ ClientRoomJoin newRoom roomQueue
        )
  RoomPlayerLeave client@Client {clientQueue} ->
    let (removeClient, roomEvent) =
          case Room.getPlayer client room of
            Just Player {playerName} ->
              (Room.removePlayer, OutboundPlayerLeave playerName)
            _ ->
              (Room.removePendingClient, OutboundPendingClientLeave)

        newRoom =
          removeClient client room

        (lobbyEvent, roomAction) =
          if Room.isEmpty newRoom
            then
              ( LobbyRoomRemove,
                exitSuccess
              )
            else
              ( LobbyRoomUpdate,
                broadcastRoom newRoom roomEvent
              )
     in ( newRoom,
          do
            STM.atomically $ do
              emit lobbyQueue $ lobbyEvent newRoom
              emit lobbyQueue $ LobbyClientJoin client
              emit clientQueue ClientRoomLeave
            roomAction -- if broadcast then perform atomically ?
        )
  RoomPlayerSetName (PSN {psnPlayerName = playerName}) client ->
    if Room.hasPlayerName playerName room
      then
        ( room,
          errorToClient client RoomHasPlayer
        )
      else case Room.registerPendingClient client playerName room of
        Just newRoom ->
          ( newRoom,
            broadcastRoom room $ OutboundPlayerJoin playerName
          )
        _ ->
          ( room,
            errorToClient client PlayerNameAlreadySet
          )

--------------------------------------------------------------------------------
clientHandler :: Context -> Handler Client
clientHandler context@Context {contextLobbyQueue = lobbyQueue} client@Client {clientConnection, clientId, clientRoomQueue} = \case
  ClientMessageSend message ->
    ( client,
      toConnection clientConnection message
    )
  ClientRoomJoin room roomQueue ->
    ( Client.joinRoom roomQueue client,
      toClient client $ OutboundRoomJoin room
    )
  ClientRoomLeave ->
    ( Client.leaveRoom client,
      toClient client OutboundRoomLeave
    )
  ClientDisconnect ->
    ( client,
      do
        logInfo context $
          Text.unlines
            [ "Client #" <> clientId <> " disconnected",
              "Ending client process..."
            ]

        let toEmit =
              case clientRoomQueue of
                Just roomQueue ->
                  emitIO roomQueue . RoomPlayerLeave
                _ ->
                  emitIO lobbyQueue . LobbyClientLeave

        toEmit client
        exitSuccess
    )
  ClientMessageReceive received ->
    ( client,
      case received of
        Left decodeError ->
          errorToClient client $ MessageInvalid decodeError
        Right message ->
          let withClient =
                case clientRoomQueue of
                  Just roomQueue ->
                    whenInRoom roomQueue message
                  _ ->
                    whenInLobby message
           in withClient client
    )
  where
    whenInLobby :: Message Inbound -> Client -> IO ()
    whenInLobby = \case
      InboundRoomMake roomMake ->
        emitIO lobbyQueue . LobbyRoomMake roomMake
      InboundRoomJoin roomJoin ->
        emitIO lobbyQueue . LobbyRoomJoin roomJoin
      _ ->
        flip errorToClient ClientNotInLobby

    whenInRoom :: EventQueue Room -> Message Inbound -> Client -> IO ()
    whenInRoom roomQueue = \case
      InboundRoomLeave ->
        emitIO roomQueue . RoomPlayerLeave
      InboundPlayerSetName playerSetName ->
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
      let (newModel, action) = handler model event

      action
      loop newModel
