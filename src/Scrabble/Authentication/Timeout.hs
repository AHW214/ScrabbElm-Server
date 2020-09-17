module Scrabble.Authentication.Timeout
  ( Timeout,
    cancel,
    create,
  )
where

import RIO hiding (cancel)
import qualified UnliftIO.Async as Async

newtype Timeout a = Timeout (Async a)
  deriving (Eq, Ord, Functor)

cancel :: MonadIO m => Timeout a -> m ()
cancel (Timeout t) = Async.cancel t

create :: MonadUnliftIO m => Int -> m a -> m (Timeout a)
create microseconds action =
  Timeout <$> Async.async (threadDelay microseconds >> action)
