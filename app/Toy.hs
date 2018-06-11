{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}

module Toy
    (
      AdderF
    , AdderT
    , interpret
    , total
    , add
    , findLimit
    ) where

import Control.Monad.Free (liftF, MonadFree)
import Control.Monad.Trans.Free (FreeF(Pure, Free), FreeT, runFreeT)
import Control.Monad.State

data AdderF k =
    Add Int (Bool -> k)
  | Clear k
  | Total (Int -> k)
  deriving (Functor)

type AdderT m a = FreeT AdderF m a
type ToyState = Int
type M = StateT ToyState IO

add :: MonadState ToyState m => Int -> AdderT m Bool
add a = liftF $ Add a id

clear :: MonadState ToyState m => AdderT m ()
clear = liftF $ Clear ()

total :: MonadState ToyState m => AdderT m Int
total = liftF $ Total id

findLimit :: MonadState ToyState m => AdderT m Int
findLimit = do
  t <- total
  clear
  l <- findLimit'
  clear
  add t
  return l

findLimit' :: MonadState ToyState m => AdderT m Int
findLimit' = do
   r <- add 1
   if r then findLimit' else total

interpret :: ToyState -> Int -> AdderT M r -> IO r
interpret l count a =
  evalStateT p l where
    p = do
      limit <- get
      mr <- runFreeT a
      case mr of
        Pure r -> return r
        Free (Add x k) ->
          let
            count' = x + count
            test = count' <= limit
            next = if test then count' else count
          in
            liftIO $ interpret l next (k test)
        Free (Clear k) ->
          liftIO $ interpret l 0 k
        Free (Total k) ->
          liftIO $ interpret l count (k count)
