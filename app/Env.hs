module Env
  ( envOptional,
    envOptionalWith,
    envWith,
  )
where

import Configuration.Dotenv ()
import Control.Monad.Except (throwError)
import Data.Typeable (typeOf)
import RIO
import System.Environment.Blank (getEnv)
import System.Envy (Parser, Var (..))

-- | Reference: https://github.com/dmjio/envy/blob/master/src/System/Envy.hs
envOptional :: Var a => String -> Parser (Maybe a)
envOptional key =
  flip envOptionalWith key $ \val ->
    case fromVar val of
      Nothing ->
        Left $
          "Parse failure: could not parse variable "
            <> key
            <> " into type "
            <> show (typeOf val)
      Just parsed ->
        Right parsed

envOptionalWith :: (String -> Either String a) -> String -> Parser (Maybe a)
envOptionalWith parseVar =
  envWithBase
    (pure Nothing)
    (second Just . parseVar)

envWith :: (String -> Either String a) -> String -> Parser a
envWith parseVar key =
  envWithBase
    (throwError $ "Variable not found for: " <> key)
    parseVar
    key

envWithBase :: Parser a -> (String -> Either String a) -> String -> Parser a
envWithBase varNotFound parseVar key =
  liftIO (getEnv key) >>= \case
    Nothing ->
      varNotFound
    Just val ->
      case parseVar val of
        Left err ->
          throwError err
        Right parsed ->
          pure parsed
