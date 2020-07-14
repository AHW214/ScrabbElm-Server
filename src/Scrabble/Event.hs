module Scrabble.Event
  ( emit
  , emitIO
  , pass
  , passIO
  ) where


--------------------------------------------------------------------------------
import           Control.Concurrent.STM (STM, TBQueue)

import           Scrabble.Types         (Client, Event (..))

import qualified Control.Concurrent.STM as STM


--------------------------------------------------------------------------------
passIO :: TBQueue (Event i e) -> Client -> e -> IO ()
passIO queue client = STM.atomically . pass queue client


--------------------------------------------------------------------------------
pass :: TBQueue (Event i e) -> Client -> e -> STM ()
pass queue client = STM.writeTBQueue queue . EventExternal client


--------------------------------------------------------------------------------
emitIO :: TBQueue (Event i e) -> i -> IO ()
emitIO queue = STM.atomically . emit queue


--------------------------------------------------------------------------------
emit :: TBQueue (Event i e) -> i -> STM ()
emit queue = STM.writeTBQueue queue . EventInternal
