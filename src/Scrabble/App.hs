module Scrabble.App
  ( App (..),
    HasPendingClients (..),
    addPendingClient,
    newPendingClients,
    removePendingClient,
  )
where

import RIO
import RIO.Process (HasProcessContext (..), ProcessContext)
import qualified RIO.Set as Set
import Scrabble.Client (Client)
import Scrabble.Common

data App = App
  { appLogFunc :: !LogFunc,
    appPendingClients :: !(TMVar (Set (ID Client))),
    appProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

class HasPendingClients env where
  pendingClientsL :: SimpleGetter env (TMVar (Set (ID Client)))

instance HasPendingClients App where
  pendingClientsL = to appPendingClients

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

addPendingClient :: HasPendingClients env => ID Client -> RIO env ()
addPendingClient clientId = do
  pendingClients <- view pendingClientsL
  liftIO $ atomically $ modifyTMVar' pendingClients $ Set.insert clientId

removePendingClient :: HasPendingClients env => ID Client -> RIO env Bool
removePendingClient clientId = do
  pendingClients <- view pendingClientsL
  liftIO $
    atomically $
      stateTMVar pendingClients $ \pending ->
        if Set.member clientId pending
          then (True, Set.delete clientId pending)
          else (False, pending)

newPendingClients :: IO (TMVar (Set (ID Client)))
newPendingClients = newTMVarIO Set.empty
