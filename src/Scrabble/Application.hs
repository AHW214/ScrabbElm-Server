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
import           Scrabble.Message         (Inbound, JoinRoom (..), Message (..), MakeRoom (..), Outbound)
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
runClient server@Server { lobbyBroadcastChan } client@Client { clientMessageChan } = do
  void $ Async.race lobbyProcess receiveProcess
  where
    lobbyProcess :: IO ()
    lobbyProcess = join $ STM.atomically $ do
      message <- readEitherMessage
      case message of
        Left broadcast ->
          pure $ do
            sendToClient client broadcast
            lobbyProcess

        Right inbound -> do
          result <- handleLobby server client inbound
          pure $ case result of
            EnterRoom room ->
              roomProcess room

            LobbySendError err ->
              sendToClient client (CausedError err) >> lobbyProcess

            LobbyNoOp ->
              lobbyProcess

    roomProcess :: Room -> IO ()
    roomProcess room =
      join $ STM.atomically $ do
        inbound <- readInboundMessage
        result <- handleRoom room client inbound
        pure $ case result of
          EnterLobby ->
            lobbyProcess

          RoomSendError err ->
            sendToClient client (CausedError err) >> roomProcess room

          RoomNoOp ->
            roomProcess room

    readEitherMessage :: STM (Either (Message Outbound) (Message Inbound))
    readEitherMessage = fmap Left readBroadcastMessage `STM.orElse` fmap Right readInboundMessage

    readBroadcastMessage :: STM (Message Outbound)
    readBroadcastMessage = STM.readTChan lobbyBroadcastChan

    readInboundMessage :: STM (Message Inbound)
    readInboundMessage = STM.readTChan clientMessageChan

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
  MakeRoom (MR { mrRoomCapacity = capacity, mrRoomId = roomId }) -> do
    rooms <- STM.readTVar serverRooms
    if Map.member roomId rooms then
      pure $ LobbySendError RoomAlreadyExists
    else do
      newRoom <- Room.new capacity roomId
      let newRooms = Map.insert roomId newRoom rooms
      STM.writeTVar serverRooms newRooms
      STM.writeTChan lobbyBroadcastChan $ MadeRoom roomId
      pure LobbyNoOp

  JoinRoom (JR { jrRoomId = roomId }) -> do
    rooms <- STM.readTVar serverRooms
    case Map.lookup roomId rooms of
      Just room -> do
        Room.addPlayer client "cool name" room
        pure $ EnterRoom room

      _ ->
        pure $ LobbySendError RoomNotFound


--------------------------------------------------------------------------------
handleRoom :: Room -> Client -> Message Inbound -> STM RoomAction
handleRoom room client = undefined


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
