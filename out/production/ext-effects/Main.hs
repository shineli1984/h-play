module Main where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State

data AppState
data AppEnv
data AppLog

type M = StateT AppState (ReaderT AppEnv (WriterT AppLog IO))

printReaderContent :: M ()
printReaderContent = undefined

main = do
  liftIO $ print "1"
  return ()
