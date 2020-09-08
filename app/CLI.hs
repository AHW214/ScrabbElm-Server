{-# LANGUAGE TemplateHaskell #-}

module CLI (Options (..), readOptions) where

import Options.Applicative.Simple
import qualified Paths_scrabbelm_server
import RIO
import qualified RIO.Char as Char

-- | Command-line options.
data Options = Options
  { optionsLogLevel :: !LogLevel,
    optionsPort :: !Int
  }

readOptions :: IO (Options, ())
readOptions =
  simpleOptions
    $(simpleVersion Paths_scrabbelm_server.version)
    "scrabbelm-server - The backend for a certain online word game"
    ""
    parseOptions
    empty

-- | Parse provided command-line options.
parseOptions :: Parser Options
parseOptions =
  Options
    <$> option
      parseLogLevel
      ( long "log"
          <> short 'l'
          <> help "Minimum level at which logs will be shown"
          <> showDefault
          <> value LevelInfo
          <> metavar "STR"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> help "Port on which to listen for websocket connections"
          <> showDefault
          <> value 3000
          <> metavar "INT"
      )

-- | Parse a log level from a string.
parseLogLevel :: ReadM LogLevel
parseLogLevel = eitherReader $ \level ->
  case stringToLower level of
    "debug" ->
      Right LevelDebug
    "info" ->
      Right LevelInfo
    "warn" ->
      Right LevelWarn
    "error" ->
      Right LevelError
    _ ->
      Left $ "Unknown log level: '" <> level <> "'"

-- | Convert a string to lowercase [rip in strings].
stringToLower :: String -> String
stringToLower = fmap Char.toLower
