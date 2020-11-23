{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Security.CodeGen.Types
  (Name, CodeGen, runCodeGen, newUniq, newNameWith, freshName, emitName, stmt)
  where

import           GHC.Types (Type)
import           Control.Monad.State
import           Security.Sensitivity

-- | Do not export the value contructor here:
newtype Name (s :: Sensitivity) (a :: Type) = Name String
  deriving (Eq, Ord)

type Uniq = Int

newtype CodeGen a = CodeGen (State Uniq a)
  deriving (Functor, Applicative, Monad, MonadState Uniq)

-- | NOTE: This should just be called once
runCodeGen :: CodeGen a -> a
runCodeGen (CodeGen m) = evalState m 0

newUniq :: CodeGen Uniq
newUniq = do
  curr <- get
  modify (+1)
  return curr

newNameWith :: forall s a. String -> CodeGen (Name s a)
newNameWith readableStr = do
  uniq <- newUniq
  return (Name ("_sec_" <> readableStr <> "_" <> show uniq))

freshName :: forall s a. CodeGen (Name s a)
freshName = do
  uniq <- newUniq
  return (Name ("_fresh_sec_" <> show uniq))

emitName :: forall s a. Name s a -> String
emitName (Name n) = n

stmt :: [String] -> String
stmt = (++";") . unwords

