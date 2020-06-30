module Scrabble.WebSocket
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow           (left)
import           Control.Concurrent      (MVar, modifyMVar_, readMVar)
import           Control.Exception       (finally)
import           Control.Monad           (forever)
import           Data.Text               (Text)
import           Network.WebSockets      (Connection, ServerApp)

import           Scrabble.Client         (Client (..))
import           Scrabble.Message        (Message (..), ClientMessage (..),
                                          ServerMessage (..))
import           Scrabble.Server         (Server (..))

import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import qualified Network.WebSockets      as WS

import qualified Scrabble.Authentication as Auth
import qualified Scrabble.Player         as Player
import qualified Scrabble.Room           as Room
import qualified Scrabble.Server         as Server


--------------------------------------------------------------------------------
data Error
  = AuthFormatInvalid
  | AuthTicketInvalid
  | AuthIdentityInvalid
  | MessageInvalid Text
  | RoomExists
  | RoomFull
  | RoomHasPlayer
  | RoomInGame
  | RoomInvalidCapacity
  | RoomMissingPlayer
  | RoomNoEntry
  deriving Show


--------------------------------------------------------------------------------
app :: MVar Server -> ServerApp
app mServer pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (pure ()) $
    authenticate connection
    <$> WS.receiveData connection
    <*> readMVar mServer
    >>= \case
          Right ( server, client@Client { clientId } ) ->
            finally (onConnect >> onMessage) onDisconnect
            where
              onConnect :: IO ()
              onConnect = do
                modifyMVar_ mServer $ pure . const server
                Text.putStrLn $ "Client " <> clientId <> " connected"
                toClient client $ ServerListRooms server

              onMessage :: IO ()
              onMessage = forever $ do
                message <- receiveMessage client

                modifyMVar_ mServer $ \s ->
                  case message >>= handleMessage client s of
                    Left err ->
                      -- Text.putStrLn errMsg (add logging levels)
                      toClient client (ServerError $ Text.pack $ show err)
                      >> pure s

                    Right ( s', action ) ->
                      action >> pure s'

              onDisconnect :: IO ()
              onDisconnect = do
                modifyMVar_ mServer $ pure . Server.removeConnectedClient client
                Text.putStrLn $ "Client " <> clientId <> " disconnected"

          Left err ->
            closeConnection (Text.pack $ show err) connection


--------------------------------------------------------------------------------
authenticate :: Connection -> Text -> Server -> Either Error ( Server, Client )
authenticate clientConn clientJWT server@Server { serverAuthSecret } =
  case Auth.verifyClientJWT serverAuthSecret clientJWT of
    Just ( clientId, clientTicket ) ->
      case Server.getPendingClient clientId server of
        Nothing ->
          Left AuthIdentityInvalid

        Just ticket | ticket /= clientTicket ->
          Left AuthTicketInvalid

        _ ->
          Right $ Server.acceptPendingClient clientId clientConn server

    _ ->
      Left AuthFormatInvalid


--------------------------------------------------------------------------------
receiveMessage :: Message m => Client -> m (Either Error ClientMessage)
receiveMessage =
  fmap (left MessageInvalid) . fromClient


--------------------------------------------------------------------------------
handleMessage
  :: Message m
  => Client
  -> Server
  -> ClientMessage
  -> Either Error ( Server, m () )
handleMessage client server message =
  case message of
    ClientNewRoom name capacity ->
      case Server.getRoom name server of
        Nothing ->
          if capacity > 0 && capacity <= Room.maxCapacity then
            let
              newRoom = Room.new name capacity
            in
              Right
                ( Server.addRoom newRoom server
                , broadcastLobby server $ ServerNewRoom newRoom
                )
          else
            Left RoomInvalidCapacity

        _ ->
          Left RoomExists

    ClientJoinRoom playerName roomName ->
      case Server.getRoom roomName server of
        Just room ->
          if | Room.inGame room ->
                Left RoomInGame

             | Room.isFull room ->
                Left RoomFull

             | Room.hasPlayerTag playerName room ->
                Left RoomHasPlayer

             | otherwise ->
                let
                  newPlayer = Player.new playerName client
                  newRoom = Room.addPlayer newPlayer room
                in
                  Right
                    ( Server.joinRoom client roomName
                      $ Server.addRoom newRoom server
                    , toClient client $ ServerJoinRoom newRoom
                    )

        _ ->
          Left RoomNoEntry

    ClientLeaveRoom ->
      case Server.getClientRoom client server of
        Just room ->
          case Room.getPlayer client room of
            Just player ->
              let
                maybeRoom =
                  Room.removePlayer player room

                ( update, action ) =
                  case maybeRoom of
                    Nothing ->
                      ( Server.leaveRoom client . Server.removeRoom room
                      , broadcastLobby server $ ServerRemoveRoom room
                      )
                    Just newRoom ->
                      ( Server.addRoom newRoom
                      , pure ()
                      )
              in
                Right
                  ( update server
                  , toClient client (ServerLeaveRoom room) >> action
                  )

            _ -> Left RoomMissingPlayer

        _ -> Left RoomNoEntry


--------------------------------------------------------------------------------
closeConnection :: Text -> Connection -> IO ()
closeConnection reason connection = do
  Text.putStrLn $ Text.unlines
    [ "Closing connection"
    , "Reason: " <> reason
    ]

  WS.sendClose connection reason
