{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Text.Read

type AppState = Integer
type AppEnv = Integer
type AppLog = String
newtype AppErr = NotNumber String deriving (Show)
newtype ConfigFileName = ConfigFileName String
newtype ConfigFileContent = ConfigFileContent String
type Number1 = Integer
type Number2 = Integer

type M = StateT AppState (ReaderT AppEnv (WriterT AppLog (ExceptT AppErr IO)))

-- read a file from local file system
-- read a number from the file
-- if no number than exception
-- log the number
-- use app env to provide another number
-- add these 2 numbers together to give a third number
-- store the third number in the state

readConfigFile :: ConfigFileName -> IO ConfigFileContent
readConfigFile (ConfigFileName name) = ConfigFileContent <$> readFile name

parseConfigFileContent :: MonadError AppErr m => ConfigFileContent -> m Number1
parseConfigFileContent (ConfigFileContent c) = do
  let num :: Either String Integer = readEither c
  case num of
    Left e -> throwError $ NotNumber e
    Right num -> return num

getNumberFromEnv :: MonadReader AppEnv m => m Number2
getNumberFromEnv = ask

saveNumberToState :: (MonadState AppState m) => AppState -> m ()
saveNumberToState = put

prog :: M Integer
prog = do
  fileContent <- liftIO $ readConfigFile $ ConfigFileName "./app/numberFile"
  number1 <- parseConfigFileContent fileContent
  number2 <- getNumberFromEnv
  let number3 = number1 + number2
  saveNumberToState number3
  return number3

main = do
  a <- runExceptT (runWriterT (runReaderT (runStateT prog 1) 2))
  return a
