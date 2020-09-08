-- | Entry point for the program.
module Main
  ( main,
  )
where

import CLI (Options (..), readOptions)
import RIO
import RIO.Process (mkDefaultProcessContext)
import Scrabble.App (App (..))
import Scrabble.Run (run)

-- | Run the program.
main :: IO ()
main = do
  (options, _) <- readOptions

  logOptions <- createLogOptions options
  processContext <- mkDefaultProcessContext

  withLogFunc logOptions $ \logFunc ->
    let app =
          App
            { appLogFunc = logFunc,
              appProcessContext = processContext
            }
     in runRIO app run

-- | Create log options from provided CLI options.
createLogOptions :: MonadIO m => Options -> m LogOptions
createLogOptions Options {optionsLogLevel} =
  setLogMinLevel optionsLogLevel
    . setLogUseColor True
    . setLogUseTime True
    <$> logOptionsHandle stdout False
