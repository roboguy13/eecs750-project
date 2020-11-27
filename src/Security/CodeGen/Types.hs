{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Security.CodeGen.Types
  (Name, CodeGen, runCodeGen, newUniq, newNameWith, freshName, emitName, nameSens, stmt, block)
  where

import           GHC.Types (Type)
import           Control.Monad.State
import           Security.Sensitivity
import           Security.Sing
import           Data.List

-- | Do not export the value contructor here:
data Name (s :: Sensitivity) (a :: Type) = Name (Sing s) String

instance Eq (Name s a) where
  Name _ x == Name _ y = x == y

instance Ord (Name s a) where
  compare (Name _ x) (Name _ y) = compare x y

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

newNameWith :: forall s a. SingI s => String -> CodeGen (Name s a)
newNameWith readableStr = do
  uniq <- newUniq
  return (Name sing ("_sec_" <> readableStr <> "_" <> show uniq))

freshName :: forall s a. SingI s => CodeGen (Name s a)
freshName = do
  uniq <- newUniq
  return (Name sing ("_fresh_sec_" <> show uniq))

emitName :: forall s a. Name s a -> String
emitName (Name _ n) = n

nameSens :: forall s a. Name s a -> Sing s
nameSens (Name sens _) = sens

stmt :: [String] -> String
stmt = (++";") . unwords

block :: String -> String
block = concatMap ("  "++) . intersperse "\n" . lines

