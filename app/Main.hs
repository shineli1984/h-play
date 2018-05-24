{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables, BangPatterns #-}
module Main where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Resource
import Text.Read
import System.IO

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

type M = ExceptT AppErr (StateT AppState (ReaderT AppEnv (WriterT AppLog (ResourceT IO))))

-- use lens
-- quickcheck
-- conduit?

readConfigFile :: (MonadResource m, MonadWriter AppLog m) => ConfigFileName -> m ConfigFileContent
readConfigFile (ConfigFileName name) = do
  (releaseKey, resource) <- allocate
      (openFile name ReadMode)
      (\h -> hClose h >> print "freeing resource")
  contents <- liftIO $ do
    c <- hGetContents resource
    length c `seq` return c -- hack to make this strict
  release releaseKey
  tell $ "fileContent: " ++ contents
  return $ ConfigFileContent contents


parseConfigFileContent :: (MonadError AppErr m, MonadWriter AppLog m) => ConfigFileContent -> m Number1
parseConfigFileContent (ConfigFileContent c) = do
  let num :: Either String Integer = readEither c
  case num of
    Left e -> throwError $ NotNumber e
    Right num -> do
      tell $ "number1: " ++ show num ++ "\n"
      return num

getNumberFromEnv :: (MonadReader AppEnv m, MonadWriter AppLog m) => m Number2
getNumberFromEnv = do
  num <- number1 <$> ask
  tell $ "number2: " ++ show num ++ "\n"
  return num

saveNumberToState :: (MonadState AppState m, MonadWriter AppLog m) => Integer -> m ()
saveNumberToState n = do
  tell $ show n ++ " is the result"
  put AppState{totalNumber=n}

prog :: M Integer
prog = do
  fileContent <- readConfigFile $ ConfigFileName "./app/numberFile"
  number1 <- parseConfigFileContent fileContent
  number2 <- getNumberFromEnv
  let number3 = number1 + number2
  saveNumberToState number3
  return number3

runProg =
  runResourceT .
  runWriterT .
  (\p -> runReaderT p AppEnv{number1=2}) .
  (\p -> runStateT p AppState{totalNumber=0}) .
  runExceptT

main = runProg prog
