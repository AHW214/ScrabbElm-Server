module Scrabble.Application
  ( runClient
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM   (STM, TChan, TVar)
import           Control.Monad            (forever, join, void, (<=<))
import           Data.Map                 (Map)
import           Data.Monoid              (Ap (..))
import           Data.Text                (Text)
import           Network.WebSockets       (Connection)
import           System.Exit              (exitSuccess)

import           Scrabble.Client          (Client (..))
-- import           Scrabble.Common          (ID)
import           Scrabble.Error           (Error (..))
import           Scrabble.Message         (Inbound, JoinRoom (..), Message (..), MakeRoom (..), Outbound, SetPlayerId (..))
import           Scrabble.Player          (Player (..))
import           Scrabble.Room            (Room (..))

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM   as STM
import qualified Data.Aeson               as JSON
-- import qualified Data.Foldable            as Foldable
import qualified Data.Map                 as Map
-- import qualified Data.Text                as Text
import qualified Network.WebSockets       as WS

-- import qualified Scrabble.Client          as Client
import qualified Scrabble.Room            as Room


--------------------------------------------------------------------------------
data Server = Server
  { lobbyBroadcastChan :: TChan (Message Outbound)
  , serverRooms        :: TVar (Map Text Room)
  }


--------------------------------------------------------------------------------
data LobbyAction
  = EnterRoom Room
  | LobbySendError Error -- refactor with type parameter so no duplicated constructors vs room
  | LobbyNoOp


--------------------------------------------------------------------------------
data RoomAction
  = EnterLobby
  | RoomSendError Error
  | RoomNoOp


--------------------------------------------------------------------------------
runClient :: Server -> Client -> IO ()
runClient server@Server { lobbyBroadcastChan, serverRooms } client@Client { clientMessageChan } =
  void $ Async.race (enterLobby Nothing) receiveProcess
  where
    enterLobby :: Maybe Room -> IO ()
    enterLobby fromRoom = join $ STM.atomically $ do
      rooms <-
        case fromRoom of
          Just room@Room { roomId } -> do
            roomEmpty <- Room.isEmpty room
            if roomEmpty then do
              STM.writeTChan lobbyBroadcastChan $ RemovedRoom roomId
              STM.stateTVar serverRooms $ \rooms ->
                let
                  newRooms = Map.delete roomId rooms
                in
                  ( newRooms, newRooms )
            else do
              STM.writeTChan lobbyBroadcastChan $ UpdatedRoom "room placeholder"
              STM.readTVar serverRooms

          _ ->
            STM.readTVar serverRooms

      broadcastChan <- STM.dupTChan lobbyBroadcastChan

      pure $ do
        sendToClient client $ JoinedLobby [ "room list placeholder" ] -- do something with rooms
        lobbyProcess broadcastChan

    lobbyProcess :: TChan (Message Outbound) -> IO ()
    lobbyProcess broadcastChan =
      join $ STM.atomically $ do
        message <- readBroadcastOrInbound broadcastChan
        case message of
          Left broadcast ->
            pure $ do
              sendToClient client broadcast
              lobbyProcess broadcastChan

          Right inbound -> do
            result <- handleLobby server client inbound
            pure $ case result of
              EnterRoom room ->
                enterRoom room

              LobbySendError err -> do
                sendToClient client $ CausedError err
                lobbyProcess broadcastChan

              LobbyNoOp ->
                lobbyProcess broadcastChan

    enterRoom :: Room -> IO ()
    enterRoom room@Room { roomBroadcastChan, roomId } =
      join $ STM.atomically $ do
        broadcastChan <- STM.dupTChan roomBroadcastChan

        pure $ do
          sendToClient client $ JoinedRoom roomId
          roomProcess broadcastChan room

    -- almost isomorphic to lobbyProcess; abstract it out
    roomProcess :: TChan (Message Outbound) -> Room -> IO ()
    roomProcess broadcastChan room =
      join $ STM.atomically $ do
        message <- readBroadcastOrInbound broadcastChan
        case message of
          Left broadcast ->
            pure $ do
              sendToClient client broadcast
              roomProcess broadcastChan room

          Right inbound -> do
            result <- handleRoom room client inbound
            pure $ case result of
              EnterLobby ->
                enterLobby $ Just room

              RoomSendError err -> do
                sendToClient client $ CausedError err
                roomProcess broadcastChan room

              RoomNoOp ->
                roomProcess broadcastChan room

    readBroadcastOrInbound :: TChan (Message Outbound) -> STM (Either (Message Outbound) (Message Inbound))
    readBroadcastOrInbound broadcastChan =
      fmap Left (readBroadcast broadcastChan) `STM.orElse` fmap Right readInbound

    readBroadcast :: TChan (Message Outbound) -> STM (Message Outbound)
    readBroadcast = STM.readTChan

    readInbound :: STM (Message Inbound)
    readInbound = STM.readTChan clientMessageChan

    receiveProcess :: IO ()
    receiveProcess = do
      received <- receiveFromClient client
      case received of
        Left _ ->
          sendToClient client $ CausedError (MessageInvalid "bad message silly head")

        Right message ->
          STM.atomically $ STM.writeTChan clientMessageChan message


--------------------------------------------------------------------------------
handleLobby :: Server -> Client -> Message Inbound -> STM LobbyAction
handleLobby Server { lobbyBroadcastChan, serverRooms } client = \case
  MakeRoom (MR { mrRoomCapacity = capacity, mrRoomId = roomId }) ->
    if capacity <= Room.maxCapacity && capacity > 0 then do
      rooms <- STM.readTVar serverRooms
      if Map.member roomId rooms then
        pure $ LobbySendError RoomAlreadyExists
      else do
        newRoom <- Room.new capacity roomId
        let newRooms = Map.insert roomId newRoom rooms
        STM.writeTVar serverRooms newRooms
        STM.writeTChan lobbyBroadcastChan $ MadeRoom roomId
        pure LobbyNoOp
    else
      pure $ LobbySendError RoomInvalidCapacity

  JoinRoom (JR { jrRoomId = roomId }) -> do
    rooms <- STM.readTVar serverRooms
    case Map.lookup roomId rooms of
      Just room -> do
        canJoin <- Room.canJoin room
        if canJoin then do
          Room.addPendingClient client room
          STM.writeTChan lobbyBroadcastChan $ UpdatedRoom "room placeholder"
          pure $ EnterRoom room
        else
          pure $ LobbySendError RoomCannotJoin -- vague

      _ ->
        pure $ LobbySendError RoomNotFound

  _ ->
    pure $ LobbySendError MessageInapplicable -- narrow message type per location to remove this case ?


--------------------------------------------------------------------------------
handleRoom :: Room -> Client -> Message Inbound -> STM RoomAction
handleRoom room client = \case
  JoinLobby -> do
    Room.getPlayer client room >>= \case
      Just Player { playerId } -> do
        Room.removePlayer client room
        Room.broadcast room $ PlayerLeft playerId

      _ ->
        Room.removePendingClient client room

    pure EnterLobby

  SetPlayerId (SPI { spiPlayerId = playerId }) -> do
    isPendingClient <- Room.hasPendingClient client room
    if isPendingClient then do
      nameInUse <- Room.hasPlayerWithId playerId room
      if nameInUse then
        pure $ RoomSendError PlayerIdInUse
      else do
        Room.removePendingClient client room -- todo: should probably have all stm reads first and then stm writes
        Room.addPlayer client playerId room
        Room.broadcast room $ PlayerJoined playerId -- this will tell the player that joined too [change order of these actions ?]

        pure RoomNoOp
    else
      pure $ RoomSendError PlayerIdAlreadySet

  _ ->
    pure $ RoomSendError MessageInapplicable


--------------------------------------------------------------------------------
sendToClient :: Client -> Message Outbound -> IO ()
sendToClient Client { clientConnection } =
  WS.sendTextData clientConnection . JSON.encode


--------------------------------------------------------------------------------
receiveFromClient :: Client -> IO (Either String (Message Inbound))
receiveFromClient =
  fmap JSON.eitherDecodeStrict' . WS.receiveData . clientConnection


{-
--------------------------------------------------------------------------------
messageLoop :: Connection -> TVar (Client a b) -> Server -> IO ()
messageLoop connection tClient server = forever $
  Foldable.traverse_ sendMessage
    =<< withDecoded . JSON.eitherDecodeStrict'
    =<< WS.receiveData connection
  where
    withDecoded :: Either String (Message Inbound) -> IO [ ( Transmission, Message Outbound ) ]
    withDecoded = \case
      Right message ->
        STM.atomically $ handleLobbyMessage tClient server message

      Left errorMessage ->
        pure $ err (MessageInvalid $ Text.pack errorMessage)

    sendMessage :: ( Transmission, Message Outbound ) -> IO ()
    sendMessage ( transmission, message ) =
      case transmission of
        Respond ->
          sendConnection connection message

    sendClient :: (Client a b) -> Message Outbound -> IO ()
    sendClient =
      sendConnection . clientConnection

    sendConnection :: Connection -> Message Outbound -> IO ()
    sendConnection conn =
      WS.sendTextData conn . JSON.encode


--------------------------------------------------------------------------------
handleMessage :: TVar Client -> Server -> Message Inbound -> STM [ ( Transmission, Message Outbound ) ]
handleMessage tClient Server { serverClients, serverRooms } = \case
  {-
  JoinServer clientId -> do
    tClient <- STM.newTVar Client { clientRoom = Nothing }
    roomList <- getRoomList server
    STM.stateTVar serverClients $
      ( JoinedServer roomList, ) . Map.insert clientId tClient
  -}

  MakeRoom (MR { mrRoomCapacity = capacity, mrRoomId = roomId }) -> do
    rooms <- STM.readTVar serverRooms

    if Map.member roomId rooms then
      respondError RoomAlreadyExists
    else do
      let newRoom = Room.new roomId capacity
      newRooms <- flip (Map.insert roomId) rooms <$> STM.newTVar newRoom

      STM.writeTVar serverRooms newRooms

      tellLobby $ MadeRoom newRoom

  JoinRoom (JR { jrRoomId = roomId }) -> do
    client <- STM.readTVar tClient

    if Client.inRoom client then
      respondError ClientAlreadyInRoom
    else do
      rooms <- STM.readTVar serverRooms

      case Map.lookup roomId rooms of
        Just tRoom -> do
          room <- STM.readTVar tRoom

          let newRoom   = Room.addPlayer client "room id" room
              newClient = Client.joinRoom tRoom client

          STM.writeTVar tRoom newRoom
          STM.writeTVar tClient newClient

          respond $ JoinedRoom newRoom

        _ ->
          respond $ CausedError RoomNotFound

  LeaveRoom -> do
    client <- STM.readTVar tClient

    case Client.getRoom client of
      Just tRoom -> do
        room <- STM.readTVar tRoom

        let newRoom =
              Room.removePlayer client room

            newClient =
                Client.leaveRoom client

            ( leaveAction, leaveTransmission ) =
              if Room.isEmpty newRoom then
                let
                  rid = roomId newRoom
                in
                  ( STM.modifyTVar' serverRooms $ Map.delete rid
                  , tellLobby $ RemovedRoom rid
                  )
              else
                ( STM.writeTVar tRoom newRoom
                , pure []
                )

        leaveAction
        STM.writeTVar tClient newClient

        bundle
          [ respond LeftRoom
          , leaveTransmission
          ]

      _ ->
        respondError ClientNotInRoom
  where
    respond :: Message Outbound -> STM [ ( Transmission, Message Outbound ) ]
    respond = pure . (:[]) . ( Respond, )

    respondError :: Error -> STM [ ( Transmission, Message Outbound ) ]
    respondError = pure . err

    tellLobby :: Message Outbound -> STM [ ( Transmission, Message Outbound ) ]
    tellLobby message = do
      clients <- STM.readTVar serverClients
      lobby <- filter (not . Client.inRoom) <$> traverse STM.readTVar (Map.elems clients)
      pure [ ( Broadcast lobby, message ) ]


--------------------------------------------------------------------------------
err :: Error -> [ ( Transmission, Message Outbound ) ]
err = (:[]) . ( Respond, ) . CausedError


--------------------------------------------------------------------------------
bundle :: (Foldable t, Applicative f, Monoid a) => t (f a) -> f a
bundle = getAp . foldMap Ap


--------------------------------------------------------------------------------
getRoomList :: Server -> STM [ Room ]
getRoomList =
  mapM STM.readTVar . Map.elems <=< STM.readTVar . serverRooms
-}
