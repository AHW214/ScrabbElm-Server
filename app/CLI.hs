{-# LANGUAGE TemplateHaskell #-}

-- | Command-line interface for the program.
module CLI
  ( Options (..),
    readOptions,
  )
where

import Config (Config (..), defaultConfig)
import Options.Applicative.Simple
import qualified Paths_scrabbelm_server
import RIO
import qualified RIO.List as List
import Scrabble.Common
import Scrabble.Logger (ColorOption (..), logLevelFromString)

-- | Command-line options.
data Options = Options
  { optionsColor :: !ColorOption,
    optionsConfigFile :: !(Maybe FilePath),
    optionsPort :: !(Maybe Int),
    optionsQuiet :: !Bool,
    optionsVerbose :: !Bool,
    optionsVerbosity :: !(Maybe LogLevel)
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
            ( "Whether to color log messages"
                <> renderChoices
                  [ AlwaysColor,
                    NeverColor,
                    AutoColor
                  ]
            )
          <> showDefaultWith renderColorOption
          <> value AutoColor
          <> metavar "STR"
      )
    <*> optional
      ( strOption
          ( long "config"
              <> help "Config file for specifying environment variables"
              <> metavar "STR"
          )
      )
    <*> optional
      ( option
          auto
          ( long "port"
              <> short 'p'
              <> help
                ( "Port on which to listen for websocket connections"
                    <> renderDefault
                      (configServerPort defaultConfig)
                )
              <> metavar "INT"
          )
      )
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help
            ( "Only show critical errors"
                <> exampleVerbosity
                  LevelError
            )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help
            ( "Show debug-level information"
                <> exampleVerbosity
                  LevelDebug
            )
      )
    <*> optional
      ( option
          parseLogLevel
          ( long "verbosity"
              <> help
                ( "Minimum level at which logs will be shown"
                    <> renderChoices
                      [ LevelDebug,
                        LevelInfo,
                        LevelWarn,
                        LevelError
                      ]
                    <> renderDefault
                      (configMinLogLevel defaultConfig)
                )
              <> metavar "STR"
          )
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
  " (--verbosity " <> stringDisplay logLevel <> ")"

-- | Parse a log level from a string.
parseLogLevel :: ReadM LogLevel
parseLogLevel = eitherReader $ \levelName ->
  case logLevelFromString levelName of
    Just logLevel ->
      Right logLevel
    Nothing ->
      Left $ "Unknown log level: '" <> levelName <> "'"

-- | Render a list of choices for the input to an option.
renderChoices :: Display a => [a] -> String
renderChoices choices =
  " (choices: " <> List.intercalate ", " (stringDisplay <$> choices) <> ")"

renderDefault :: Display a => a -> String
renderDefault defValue =
  " (default: " <> stringDisplay defValue <> ")"
