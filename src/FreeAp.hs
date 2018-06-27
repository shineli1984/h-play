{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}

module FreeAp where

import Control.Applicative.Free
import Data.Functor.Identity
import Control.Monad.State

type Address = String
type Name = String
type FieldName = String
type Error = String

data User = User {
    name :: Name
  , address :: Address
} deriving (Show)

conUser :: Either Error Name -> Either Error Address -> Either [Error] User
conUser (Right n) (Right a) = Right (User n a)
conUser (Left e) (Right _) = Left [e]
conUser (Right _) (Left e) = Left [e]
conUser (Left e1) (Left e2) = Left [e1, e2]

data Field a = Field
  { fName :: FieldName
  , fValidate :: String -> a
  } deriving (Functor)

type Form = Ap Field

validateUserName :: Form (Either Error Name)
validateUserName = liftAp $ Field "Name" v
  where v name = if name == "shine" then Left "User name already exists" else Right name

validateAddress :: Form (Either Error Address)
validateAddress = liftAp $ Field "Address" Right

form :: Form (Either [Error] User)
form = conUser
  <$> validateUserName
  <*> validateAddress

nt :: MonadIO m => Field a -> m a
nt (Field n v) = do
  liftIO $ print n
  x <- liftIO getLine
  return $ v x

validate :: IO (Either [Error] User)
validate = runAp nt form