{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Text.Read

data AppState
data AppEnv
type AppLog = String
newtype AppErr = NumberTooBig String
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
    Left _ -> throwError $ NumberTooBig "number too big"
    Right num -> return num

getNumberFromEnv :: MonadReader AppEnv m => m Number2
getNumberFromEnv = undefined

saveNumberToState :: (MonadState AppState m, Num a) => a -> m ()
saveNumberToState = undefined

prog :: M ()
prog = do
  fileContent <- liftIO $ readConfigFile $ ConfigFileName ""
  number1 <- parseConfigFileContent fileContent
  -- number2 <- getNumberFromEnv
  -- let number3 = number1 + number2
  -- saveNumberToState number3

  return ()

main = do
  liftIO $ print "1" :: IO ()
  return ()
