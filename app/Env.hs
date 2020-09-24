module Env
  ( EnvError (..),
    FromEnv (..),
    ToEnv (..),
    Var (..),
    decodeEnv,
    env,
    envOptional,
    envOptionalWith,
    envWith,
    loadEnvFile,
    withDefault,
  )
where

import qualified Configuration.Dotenv as Dotenv
import Control.Monad.Except (throwError)
import Data.Typeable (typeOf)
import RIO
import qualified RIO.Text as Text
import Scrabble.Common (Try (..), tries)
import System.Environment.Blank (getEnv)
import System.Envy
  ( EnvList,
    EnvVar (..),
    FromEnv (..),
    Parser,
    ToEnv (..),
    Var (..),
    env,
    (.!=),
  )
import qualified System.Envy as Envy
import System.IO.Error (isDoesNotExistError)

data EnvError
  = EnvCannotDecode Text
  | EnvFileCannotParse SomeException
  | EnvFileCannotSet Text
  | EnvFileDoesNotExist Text
  | EnvFileIOException IOException

instance Display EnvError where
  display = \case
    EnvCannotDecode reason ->
      "Cannot decode env: "
        <> display reason
    EnvFileCannotParse ex ->
      "Cannot parse env file: "
        <> display ex
    EnvFileCannotSet reason ->
      "Cannot set vars from env file: "
        <> display reason
    EnvFileDoesNotExist filePath ->
      "Env file "
        <> display filePath
        <> " does not exist"
    EnvFileIOException ex ->
      "IO exception loading env file: "
        <> display ex

decodeEnv :: (FromEnv a, MonadIO m) => m (Either EnvError a)
decodeEnv =
  first (EnvCannotDecode . Text.pack) <$> liftIO Envy.decodeEnv

withDefault :: Parser (Maybe a) -> a -> Parser a
withDefault = (.!=)

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

loadEnvFile ::
  MonadUnliftIO m =>
  FilePath ->
  m (Either EnvError ())
loadEnvFile filePath =
  parseEnvFile filePath >>= \case
    Left err ->
      pure $ Left err
    Right vars ->
      liftIO $
        fmap (first (EnvFileCannotSet . Text.pack)) $
          Envy.setEnvironment vars

parseEnvFile ::
  forall m a.
  MonadUnliftIO m =>
  FilePath ->
  m (Either EnvError (EnvList a))
parseEnvFile filePath =
  tries
    (pairsToEnvList <$> Dotenv.parseFile filePath)
    [ Try
        ( \(ex :: IOException) ->
            if isDoesNotExistError ex
              then EnvFileDoesNotExist $ Text.pack filePath
              else EnvFileIOException ex
        ),
      Try EnvFileCannotParse
    ]
  where
    pairsToEnvList :: [(String, String)] -> EnvList a
    pairsToEnvList =
      Envy.makeEnv . fmap (uncurry EnvVar)
