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
import qualified RIO.Char as Char
import qualified RIO.List as List
import qualified RIO.Map as Map

-- | Command-line options.
data Options = Options
  { optionsLogLevel :: !LogLevel,
    optionsPort :: !Int
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
      parseLogLevel
      ( long "log"
          <> short 'l'
          <> help ("Minimum level at which logs will be shown (choices: " <> logLevelChoices <> ")")
          <> showDefaultWith renderLogLevel
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
  case Map.lookup (stringToUpper level) logLevelMap of
    Just logLevel ->
      Right logLevel
    _ ->
      Left $ "Unknown log level: '" <> level <> "'"

-- | Possible log levels, rendered as a string.
logLevelChoices :: String
logLevelChoices = List.intercalate ", " $ Map.keys logLevelMap

-- | Map of names to log levels.
logLevelMap :: Map String LogLevel
logLevelMap = Map.fromList logLevelsWithNames
  where
    logLevelsWithNames :: [(String, LogLevel)]
    logLevelsWithNames = mapMaybe groupWithName logLevels

    groupWithName :: LogLevel -> Maybe (String, LogLevel)
    groupWithName logLevel = (,logLevel) <$> tryRenderLogLevel logLevel

    logLevels :: [LogLevel]
    logLevels = [LevelDebug, LevelInfo, LevelWarn, LevelError]

-- | Render a log level as as string [accounting for hopefully impossible failure].
renderLogLevel :: LogLevel -> String
renderLogLevel = fromMaybe "INVALID LOG LEVEL" . tryRenderLogLevel

-- | Try to render a log level as a string.
tryRenderLogLevel :: LogLevel -> Maybe String
tryRenderLogLevel =
  fmap stringToUpper
    . List.stripPrefix "Level"
    . show

-- | Convert a string to uppercase [rip in strings].
stringToUpper :: String -> String
stringToUpper = fmap Char.toUpper
