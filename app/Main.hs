module Main (main) where

import CLI (Options (..), readOptions)
import RIO
import RIO.Process (mkDefaultProcessContext)
import Scrabble.App (App (..))
import Scrabble.Run (run)

main :: IO ()
main = do
  (options, _) <- readOptions

  lo <- createLogOptions options
  pc <- mkDefaultProcessContext

  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc
            }
     in runRIO app run

createLogOptions :: MonadIO m => Options -> m LogOptions
createLogOptions Options {optionsLogLevel} =
  setLogMinLevel optionsLogLevel
    . setLogUseColor True
    . setLogUseTime True
    <$> logOptionsHandle stdout False
