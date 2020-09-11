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
import Scrabble.Logger (ColorOption (..), logLevelNameAndColor)

-- | Command-line options.
data Options = Options
  { optionsColor :: !ColorOption,
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
      parseColorOption
      ( long "color"
          <> short 'c'
          <> help ("Whether to color log messages " <> renderChoices colorOptions)
          <> showDefaultWith renderColorOption
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
          <> help ("Minimum level at which logs will be shown " <> renderChoices logLevels)
          <> showDefaultWith renderLogLevel
          <> value LevelInfo
          <> metavar "STR"
      )

-- | Render a color option as a string.
renderColorOption :: ColorOption -> String
renderColorOption = \case
  AlwaysColor ->
    "always"
  NeverColor ->
    "never"
  AutoColor ->
    "auto"

-- | Parse a color option from a string.
parseColorOption :: ReadM ColorOption
parseColorOption = eitherReader $ \case
  "always" ->
    Right AlwaysColor
  "never" ->
    Right NeverColor
  "auto" ->
    Right AutoColor
  colorOption ->
    Left $ "Invalid color option: '" <> colorOption <> "'"

-- | Possible color options.
colorOptions :: [String]
colorOptions =
  [ "always",
    "never",
    "auto"
  ]

-- | Render a log level as a string.
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

-- | Possible log levels.
logLevels :: [String]
logLevels =
  [ "debug",
    "info",
    "warn",
    "error"
  ]

-- | Render a list of choices for the input to an option.
renderChoices :: [String] -> String
renderChoices choices = "(choices: " <> List.intercalate ", " choices <> ")"
