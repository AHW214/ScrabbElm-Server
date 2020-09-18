-- | Simple interface for timing out actions.
module Scrabble.Authentication.Timeout
  ( Timeout,
    cancel,
    create,
    wait,
  )
where

import RIO hiding (cancel, wait)
import qualified UnliftIO.Async as Async

-- | A timed-out asynchronous action.
newtype Timeout a = Timeout (Async a)
  deriving (Eq, Ord, Functor)

-- | Cancel a running timeout.
cancel :: MonadIO m => Timeout a -> m ()
cancel (Timeout t) = Async.cancel t

-- | Await a timeout and return its output.
wait :: MonadIO m => Timeout a -> m a
wait (Timeout t) = Async.wait t

-- Create a new timeout.
create ::
  MonadUnliftIO m =>
  -- | The duration of the timeout, in microseconds.
  Int ->
  -- | The action to perform after the timeout.
  m a ->
  -- | The new timeout.
  m (Timeout a)
create microseconds action =
  Timeout <$> Async.async (threadDelay microseconds >> action)
