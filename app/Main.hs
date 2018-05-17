{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Text.Read

data AppState = AppState {
  totalNumber :: Integer
} deriving (Show)
data AppEnv = AppEnv {
  number1 :: Number1
} deriving (Show)
type AppLog = String
newtype AppErr = NotNumber String deriving (Show)
newtype ConfigFileName = ConfigFileName String
newtype ConfigFileContent = ConfigFileContent String
type Number1 = Integer
type Number2 = Integer

type M = StateT AppState (ReaderT AppEnv (WriterT AppLog (ExceptT AppErr IO)))

-- safely release resource using ResourceT?

readConfigFile :: ConfigFileName -> IO ConfigFileContent
readConfigFile (ConfigFileName name) = ConfigFileContent <$> readFile name

parseConfigFileContent :: MonadError AppErr m => ConfigFileContent -> m Number1
parseConfigFileContent (ConfigFileContent c) = do
  let num :: Either String Integer = readEither c
  case num of
    Left e -> throwError $ NotNumber e
    Right num -> return num

getNumberFromEnv :: MonadReader AppEnv m => m Number2
getNumberFromEnv = number1 <$> ask

saveNumberToState :: (MonadState AppState m) => Integer -> m ()
saveNumberToState n = put AppState{totalNumber=n}

prog :: M Integer
prog = do
  fileContent@(ConfigFileContent fc) <- liftIO $ readConfigFile $ ConfigFileName "./app/numberFile"
  tell $ "fileContent: " ++ fc
  number1 <- parseConfigFileContent fileContent
  tell $ "number1: " ++ show number1 ++ "\n"
  number2 <- getNumberFromEnv
  tell $ "number2: " ++ show number2 ++ "\n"
  let number3 = number1 + number2
  saveNumberToState number3
  tell $ show number3 ++ " is the result"
  return number3

main = do
  a <- runExceptT (runWriterT (runReaderT (runStateT prog AppState{totalNumber=0}) AppEnv{number1=2}))
  return a
