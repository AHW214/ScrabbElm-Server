module Scrabble.WebSocket
  ( app
  ) where


--------------------------------------------------------------------------------
import           Control.Arrow           (left)
import           Control.Concurrent      (MVar, modifyMVar, modifyMVar_,
                                          readMVar)
import           Control.Exception       (finally)
import           Control.Monad           (forever, join)
import           Data.Text               (Text)
import           Network.WebSockets      (Connection, ServerApp)
import           TextShow                (TextShow (..), FromStringShow (..))

import           Scrabble.Client         (Client (..))
import           Scrabble.Log            (Log (..))
import           Scrabble.Message        (Message (..), ClientEvent (..),
                                          ServerMessage (..))
import           Scrabble.Player         (Player (..))
import           Scrabble.Room           (Room (..))
import           Scrabble.Server         (Server (..))

import qualified Data.Text               as Text
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
  | ClientNotInRoom
  | MessageInvalid Text
  | PlayerNotRoomOwner
  | RoomExists
  | RoomFull
  | RoomHasPlayer
  | RoomInGame
  | RoomInvalidCapacity
  | RoomMissingPlayer
  | RoomNoEntry
  deriving Show


--------------------------------------------------------------------------------
instance TextShow Error where
  showbPrec p = showbPrec p . FromStringShow


--------------------------------------------------------------------------------
app :: MVar Server -> ServerApp
app mServer pending = do
  connection <- WS.acceptRequest pending
  WS.withPingThread connection 30 (pure ()) $ do
    authResponse <- WS.receiveData connection
    server <- readMVar mServer

    case authenticate connection authResponse server of
      Right ( newServer, client@Client { clientId } ) -> do
        modifyMVar_ mServer $ pure . const newServer
        logInfo newServer $ "Client " <> clientId <> " connected"
        toClient client $ ServerListRooms (Server.previewRooms newServer)

        finally onMessage onDisconnect
        where
          onMessage :: IO ()
          onMessage = forever $ receiveMessage >>= \case
            Left err ->
              printError newServer err

            Right msg ->
              join $ modifyMVar mServer $ \s ->
                case handleMessage client s msg of
                  Left err -> pure
                    ( s
                    , printError s err
                      >> toClient client (ServerError $ showt err)
                    )

                  Right res ->
                    pure res

          onDisconnect :: IO ()
          onDisconnect = join $ modifyMVar mServer $ \s ->
            let
              ( s', action ) =
                case Server.getPlayerInfo client s of
                  Just ( player, room ) ->
                    evictPlayer player room s

                  _ ->
                    ( s, pure () )
            in
              pure
                ( Server.removeConnectedClient client s'
                , action >> logInfo s' ("Client " <> clientId <> " disconnected")
                )

          receiveMessage :: Message m => m (Either Error ClientEvent)
          receiveMessage =
            left MessageInvalid <$> fromClient client

      Left err -> do
        let reason = showt err

        logError server $ Text.unlines
          [ "Closing pending connection"
          , "Reason: " <> reason
          ]

        WS.sendClose connection reason


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
handleMessage
  :: forall m. (Log m, Message m)
  => Client
  -> Server
  -> ClientEvent
  -> Either Error ( Server, m () )
handleMessage client server message =
  case message of
    ClientNewRoom roomName capacity playerName ->
      case Server.getRoom roomName server of
        Nothing ->
          if capacity > 0 && capacity <= Room.maxCapacity then
            let
              owner = Player.new playerName client
              newRoom = Room.new roomName capacity owner
              newRoomPreview = Room.toPreview newRoom
            in
              Right
                ( Server.addRoom newRoom server
                , broadcastLobby server (ServerNewRoom newRoomPreview) -- exclude client who made room ?
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
                    , broadcastRoom newRoom (ServerPlayerJoinRoom playerName)
                      >> toClient client (ServerJoinRoom newRoom)
                    )

        _ ->
          Left RoomNoEntry

    ClientLeaveRoom ->
      case Server.getPlayerInfo client server of
        Just ( player, room ) ->
          Right $ evictPlayer player room server

        _ ->
          Left ClientNotInRoom

    ClientStartGame ->
      case Server.getPlayerInfo client server of
        Nothing ->
          Left ClientNotInRoom

        Just ( player, room ) | Room.isPlayerOwner player room ->
          let
            newRoom = Room.switchTurn player room
          in
            Right
              ( Server.addRoom newRoom server
              , pure () -- send racks
              )

        _ ->
          Left PlayerNotRoomOwner


--------------------------------------------------------------------------------
evictPlayer :: Message m => Player -> Room -> Server -> ( Server, m () )
evictPlayer player@Player { playerClient, playerName } room@Room { roomName } server =
  ( update server
  , toClient playerClient (ServerLeaveRoom roomName) >> action -- dont message client if on disconnect
  )
  where
    ( update, action ) =
      case Room.removePlayer player room of
        Nothing ->
          ( Server.leaveRoom playerClient . Server.removeRoom room
          , broadcastLobby server $ ServerRemoveRoom roomName
          )
        Just newRoom ->
          ( Server.addRoom newRoom
          , broadcastRoom newRoom (ServerPlayerLeaveRoom playerName)
          )
