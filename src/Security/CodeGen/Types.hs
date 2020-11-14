{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Security.CodeGen.Types where

import           GHC.Types (Type)
import           Control.Monad.State
import           Security.Sensitivity

-- | Do not export the value contructor here:
newtype Name (s :: Sensitivity) (a :: Type) = Name String

type Uniq = Int

newtype CodeGen a = CodeGen (State Uniq a)
  deriving (Functor, Applicative, Monad, MonadState Uniq)

newUniq :: CodeGen Uniq
newUniq = do
  curr <- get
  modify (+1)
  return curr

newNameWith :: forall s a. String -> CodeGen (Name s a)
newNameWith readableStr = do
  uniq <- newUniq
  return (Name ("_sec_" <> readableStr <> "_" <> show uniq))

