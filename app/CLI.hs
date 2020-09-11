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
          <> help
            ( "Whether to color log messages "
                <> renderChoices
                  renderColorOption
                  [ AlwaysColor,
                    NeverColor,
                    AutoColor
                  ]
            )
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
          <> help
            ( "Only show critical errors "
                <> exampleVerbosity
                  LevelError
            )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help
            ( "Show debug-level information "
                <> exampleVerbosity
                  LevelDebug
            )
      )
    <*> option
      parseLogLevel
      ( long "verbosity"
          <> help
            ( "Minimum level at which logs will be shown "
                <> renderChoices
                  renderLogLevel
                  [ LevelDebug,
                    LevelInfo,
                    LevelWarn,
                    LevelError
                  ]
            )
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

-- | Render an example verbosity option with the given log level.
exampleVerbosity :: LogLevel -> String
exampleVerbosity logLevel =
  "(--verbosity " <> renderLogLevel logLevel <> ")"

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

-- | Render a list of choices for the input to an option.
renderChoices :: (a -> String) -> [a] -> String
renderChoices render choices =
  "(choices: " <> List.intercalate ", " (render <$> choices) <> ")"
