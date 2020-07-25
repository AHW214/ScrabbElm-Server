{-# LANGUAGE TypeFamilies #-}

module Scrabble.Application
  ( handleMessage
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (STM, TVar)
import           Control.Monad          (forever, (<=<))
import           Data.Map               (Map)
import           Data.Monoid            (Ap (..))
import           Network.WebSockets     (Connection)

import           Scrabble.Client        (Client (..))
import           Scrabble.Common        (ID)
import           Scrabble.Error         (Error (..))
import           Scrabble.Message       (Inbound, JoinRoom (..), Message (..), MakeRoom (..), Outbound)
import           Scrabble.Room          (Room (..))

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson             as JSON
import qualified Data.Foldable          as Foldable
import qualified Data.Map               as Map
import qualified Data.Text              as Text
import qualified Network.WebSockets     as WS

import qualified Scrabble.Client        as Client
import qualified Scrabble.Room          as Room


--------------------------------------------------------------------------------
data Server = Server
  { serverClients :: TVar (Map (ID Client) (TVar Client))
  , serverRooms   :: TVar (Map (ID Room) (TVar Room))
  }


--------------------------------------------------------------------------------
data Transmission
  = Respond
  | Broadcast [ Client ]


--------------------------------------------------------------------------------
messageLoop :: Connection -> TVar Client -> Server -> IO ()
messageLoop connection tClient server = forever $
  Foldable.traverse_ sendMessage
    =<< withDecoded . JSON.eitherDecodeStrict'
    =<< WS.receiveData connection
  where
    withDecoded :: Either String (Message Inbound) -> IO [ ( Transmission, Message Outbound ) ]
    withDecoded = \case
      Right message ->
        STM.atomically $ handleMessage tClient server message

      Left errorMessage ->
        pure $ err (MessageInvalid $ Text.pack errorMessage)

    sendMessage :: ( Transmission, Message Outbound ) -> IO ()
    sendMessage ( transmission, message ) =
      case transmission of
        Respond ->
          sendConnection connection message

        Broadcast clients ->
          Foldable.traverse_ (flip sendClient message) clients

    sendClient :: Client -> Message Outbound -> IO ()
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
