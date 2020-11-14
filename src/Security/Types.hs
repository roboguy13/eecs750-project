{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Security.Types
  where

import           Prelude hiding ((>>), (>>=), return)
import           GHC.Types (Type)

data Sensitivity = Public | Secret

type family Max sA sB where
  Max Public Public = Public
  Max sA     sB     = Secret

type family Min sA sB where
  Min Secret Secret = Secret
  Min sA     sB     = Public

type family (:<=) sA sB :: Bool where
  (:<=) Public sB     = 'True
  (:<=) Secret Secret = 'True
  (:<=) Secret Public = 'False

newtype Name (s :: Sensitivity) (a :: Type) = Name String

data Ptr a
data Array a

-- TODO: Add a mechanism for "Secret" lambdas?
data Cmd (s :: Sensitivity) a where
  Nop :: forall s. Cmd s ()

  Seq :: forall sA sB a b. Cmd sA a -> Cmd sB b -> Cmd (Max sA sB) b

  Bind :: forall sA sB a b. (sA :<= sB) ~ True => Cmd sA a -> (a -> Cmd sB b) -> Cmd (Max sA sB) b

  AllocSecret :: forall a. Int -> Cmd Secret (Array a)
  AllocPublic :: forall a. Int -> Cmd Public (Array a)

  Decl :: forall s a. a -> Cmd s (Name s a)
  Assign :: forall s a. Expr s a -> Expr s a -> Cmd s ()  -- | memcpy's arrays

  NameFFI :: forall a. String -> Cmd Public a

  IfThenElse :: forall s a. Expr s Bool -> Cmd s a -> Cmd s a -> Cmd s a
  While :: forall s a. Expr s Bool -> Cmd s a -> Cmd s ()
  -- For :: forall s a. String -> 


pattern x := y = Assign x y


(>>=) :: forall sA sB a b. (sA :<= sB) ~ True => Cmd sA a -> (a -> Cmd sB b) -> Cmd (Max sA sB) b
(>>=) = Bind

(>>) = Seq

-- type LVal s a = forall side. Expr side s a


test :: Cmd Public ()
test = do
  x <- Decl @Public (1 :: Int)
  let xVar = Var x
      y = xVar + xVar

  xVar := y

  Nop @Public


data Expr  (s :: Sensitivity) a where
  Literal :: forall s a. a -> Expr s a
  -- LValExpr :: forall s a. LVal s a -> Expr s a

  Add :: forall s a. Num a => Expr s a -> Expr s a -> Expr s a
  Sub :: forall s a. Num a => Expr s a -> Expr s a -> Expr s a
  Mul :: forall s a. Num a => Expr s a -> Expr s a -> Expr s a

  Eql :: forall s a. Eq a => Expr s a -> Expr s a -> Expr s Bool
  Lt :: forall s a. Ord a => Expr s a -> Expr s a -> Expr s Bool
  Gt :: forall s a. Ord a => Expr s a -> Expr s a -> Expr s Bool

  Not :: forall s. Expr s Bool -> Expr s Bool
  And :: forall s. Expr s Bool -> Expr s Bool -> Expr s Bool
  Or :: forall s. Expr s Bool -> Expr s Bool -> Expr s Bool

  Call :: forall s a b. Name s (a -> b) -> Expr s a -> Expr s b

  Var :: forall s a. Name s a -> Expr s a -- Do not export this constructor
  Deref :: forall s a. Expr s (Ptr a) -> Expr s a
  Index :: forall sA sB a. (sB :<= sA) ~ True => Expr sA (Array a) -> Expr sB Int -> Expr sA a


(==?), (<?), (>?) :: forall s a. Ord a => Expr s a -> Expr s a -> Expr s Bool
(==?) = Eql
(<?) = Lt
(>?) = Gt

(&&?), (||?) :: forall s. Expr s Bool -> Expr s Bool -> Expr s Bool
(&&?) = And
(||?) = Or

(+=) :: forall s a. Num a => Expr s a -> Expr s a -> Cmd s ()
x += y = x := (x + y)

(-=) :: forall s a. Num a => Expr s a -> Expr s a -> Cmd s ()
x -= y = x := (x - y)


instance (Num a) => Num (Expr s a) where
  (+) = Add
  (*) = Mul
  (-) = Sub
  fromInteger = Literal . fromInteger

