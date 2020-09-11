{-# LANGUAGE TemplateHaskell #-}

-- | Command-line interface for the program.
module CLI
  ( Options (..),
    readOptions,
  )
where

import Options.Applicative.Simple
import qualified Paths_scrabbelm_server
import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import Scrabble.Logger (LogColor (..), logLevelNameAndColor)

-- | Command-line options.
data Options = Options
  { optionsColor :: !LogColor,
    optionsPort :: !Int,
    optionsQuiet :: !Bool,
    optionsVerbose :: !Bool,
    optionsVerbosity :: !LogLevel
  }

-- | Read options from the command-line.
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
      parseLogColor
      ( long "color"
          <> short 'c'
          <> help ("Whether to color log messages " <> renderChoices logColorOptions)
          <> showDefaultWith renderLogColor
          <> value AutoColor
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
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "Only show critical errors (--verbosity error)"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Show debug-level logs (--verbosity debug)"
      )
    <*> option
      parseLogLevel
      ( long "verbosity"
          <> help ("Minimum level at which logs will be shown " <> renderChoices logLevelOptions)
          <> showDefaultWith renderLogLevel
          <> value LevelInfo
          <> metavar "STR"
      )

renderLogColor :: LogColor -> String
renderLogColor = \case
  AlwaysColor ->
    "always"
  NeverColor ->
    "never"
  AutoColor ->
    "auto"

parseLogColor :: ReadM LogColor
parseLogColor = eitherReader $ \case
  "always" ->
    Right AlwaysColor
  "never" ->
    Right NeverColor
  "auto" ->
    Right AutoColor
  colorOption ->
    Left $ "Invalid color option: '" <> colorOption <> "'"

logColorOptions :: [String]
logColorOptions =
  [ "always",
    "never",
    "auto"
  ]

renderLogLevel :: LogLevel -> String
renderLogLevel =
  Text.unpack
    . Text.toLower
    . textDisplay
    . fst
    . logLevelNameAndColor

-- | Parse a log level from a string.
parseLogLevel :: ReadM LogLevel
parseLogLevel = eitherReader $ \case
  "debug" ->
    Right LevelDebug
  "info" ->
    Right LevelInfo
  "warn" ->
    Right LevelWarn
  "error" ->
    Right LevelError
  level ->
    Left $ "Unknown log level: '" <> level <> "'"

logLevelOptions :: [String]
logLevelOptions =
  [ "debug",
    "info",
    "warn",
    "error"
  ]

renderChoices :: [String] -> String
renderChoices choices = "(choices: " <> List.intercalate ", " choices <> ")"
