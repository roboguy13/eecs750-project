{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Security.CodeGen.Types
  (Name, CodeGen, runCodeGen, newUniq, newNameWith, freshName, emitName, nameSens, stmt, block, SomeName, mkSomeName, someNameSens, withSomeName, emitSomeName, emitSomeNameDOT)
  where

import           GHC.Types (Type)
import           Control.Monad.State
import           Security.Sensitivity
import           Security.Sing
import           Data.List

-- | Do not export the value contructor here:
data Name (s :: Sensitivity) (a :: Type) = Name (Sing s) String

instance Show (Name s a) where
  show (Name sens n) = "(Name " ++ show (fromSing sens) ++ " " ++ show n ++ ")"

data SomeName = forall s a. SomeName (Name s a)

deriving instance Show SomeName

instance Eq SomeName where
  SomeName (Name _ x) == SomeName (Name _ y) = x == y

instance Ord SomeName where
  compare (SomeName (Name _ x)) (SomeName (Name _ y)) = compare x y

mkSomeName :: Name s a -> SomeName
mkSomeName = SomeName

someNameSens :: SomeName -> Sensitivity
someNameSens (SomeName n) = fromSing $ nameSens n

withSomeName :: SomeName -> (forall s a. Name s a -> r) -> r
withSomeName (SomeName n) f = f n

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
  return (Name sing ("_s_" <> readableStr <> "_" <> show uniq))

freshName :: forall s a. SingI s => CodeGen (Name s a)
freshName = do
  uniq <- newUniq
  return (Name sing ("_fs_" <> show uniq))

emitName :: forall s a. Name s a -> String
emitName (Name _ n) = n

emitSomeName :: SomeName -> String
emitSomeName (SomeName n) = emitName n

emitSomeNameDOT :: SomeName -> String
emitSomeNameDOT sn =
  case someNameSens sn of
    Secret -> unwords [emitSomeName sn, "[shape=box]"]
    Public -> emitSomeName sn

nameSens :: forall s a. Name s a -> Sing s
nameSens (Name sens _) = sens

stmt :: [String] -> String
stmt = (++";") . unwords

block :: String -> String
block = concatMap ("  "++) . intersperse "\n" . lines

