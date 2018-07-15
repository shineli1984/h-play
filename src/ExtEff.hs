{-# LANGUAGE TypeOperators, FlexibleContexts, MonoLocalBinds, TemplateHaskell, ScopedTypeVariables, DataKinds #-}

module ExtEff (runProgram) where

import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Eff.Writer.Lazy
import Control.Eff.State.Lazy
import Control.Eff.Exception
import Control.Lens

data AppState = AppState {
  _totalNumber :: Integer
} deriving (Show)

data AppEnv = AppEnv {
  _number1 :: Integer
} deriving (Show)

type AppLog = String

newtype AppErr = NotNumber String deriving (Show)

makeLenses ''AppState
makeLenses ''AppEnv

program :: Eff '[Reader AppEnv, State AppState, Exc AppErr, Writer AppLog] Integer
program = do
  e <- view number1 <$> ask
  tell $ "got env " ++ show e

  throwError $ NotNumber "not a number"

  let r = e + 1
  s <- get
  tell $ "got state " ++ show s

  put $ s & totalNumber .~ r
  tell $ "put to state " ++ show s

  return e

runProgram = run . runWriter (++) "" . runError . runState (AppState 0) . runReader (AppEnv 1) $ program
