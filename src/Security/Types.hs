{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Security.Types
  where

import           GHC.Types (Type)
import           Control.Monad.Operational
import           Security.Repr
import           Security.Sensitivity
import           Security.CodeGen.Types


-- TODO: Add a mechanism for "Secret" lambdas?
data CmdF a where
  AllocSecret :: forall a. Repr a => Int -> CmdF (Expr Secret (Ptr a))
  AllocPublic :: forall a. Repr a => Int -> CmdF (Expr Public (Ptr a))

  Decl :: forall a. Repr a => a -> CmdF (Name Public a)
  Assign :: forall s a. Repr a => Expr s a -> Expr s a -> CmdF ()  -- | memcpy's arrays
  -- Memcpy :: forall s a. Expr s (Array a) -> Expr s (Array a) ->

  NameFFI :: forall a. String -> CmdF a

  IfThenElse :: forall s a. Repr a => Expr s Bool -> Cmd a -> Cmd a -> CmdF a
  While :: forall s a. Repr a => Expr s Bool -> Cmd a -> CmdF ()
  For :: forall s a. Repr a => a -> (Name s a -> Expr s Bool) -> (Name s a -> Cmd ()) -> (Name s a -> Cmd ()) -> CmdF ()

type Cmd = Program CmdF

allocSecret :: forall a. Repr a => Int -> Cmd (Expr Secret (Ptr a))
allocSecret = singleton . AllocSecret @a

allocPublic :: forall a. Repr a => Int -> Cmd (Expr Public (Ptr a))
allocPublic = singleton . AllocPublic @a

decl :: forall a. Repr a => a -> Cmd (Name Public a)
decl = singleton . Decl @a

infixr 0 .=
(.=) :: forall s a. Repr a => Expr s a -> Expr s a -> Cmd ()
x .= y = singleton (Assign x y)

nameFFI :: forall a. String -> Cmd a
nameFFI = singleton . NameFFI @a

ifThenElse :: forall s a. Repr a => Expr s Bool -> Cmd a -> Cmd a -> Cmd a
ifThenElse c t f = singleton (IfThenElse c t f)

while :: forall s a. Repr a => Expr s Bool -> Cmd a -> Cmd ()
while c b = singleton (While c b)

for :: forall s a. Repr a => a -> (Name s a -> Expr s Bool) -> (Name s a -> Cmd ()) -> (Name s a -> Cmd ()) -> Cmd ()
for initial conditional update body = singleton (For initial conditional update body)


-- type LVal s a = forall side. Expr side s a

(+=) :: forall s a. (Repr a, Num a) => Expr s a -> Expr s a -> Cmd ()
x += y = x .= x + y

(-=) :: forall s a. (Repr a, Num a) => Expr s a -> Expr s a -> Cmd ()
x -= y = x .= x - y

test :: Cmd ()
test = do
  x <- decl (1 :: Int)
  let xVar = var x
      y = xVar + xVar

  xVar .= y

  return ()


data Expr  (s :: Sensitivity) a where
  Literal :: forall s a. Repr a => a -> Expr s a
  -- LValExpr :: forall s a. LVal s a -> Expr s a

  Add :: forall s a. (Repr a, Num a) => Expr s a -> Expr s a -> Expr s a
  Sub :: forall s a. (Repr a, Num a) => Expr s a -> Expr s a -> Expr s a
  Mul :: forall s a. (Repr a, Num a) => Expr s a -> Expr s a -> Expr s a

  Eql :: forall s a. (Repr a, Eq a) => Expr s a -> Expr s a -> Expr s Bool
  Lt :: forall s a. (Repr a, Ord a) => Expr s a -> Expr s a -> Expr s Bool
  Gt :: forall s a. (Repr a, Ord a) => Expr s a -> Expr s a -> Expr s Bool

  Not :: forall s. Expr s Bool -> Expr s Bool
  And :: forall s. Expr s Bool -> Expr s Bool -> Expr s Bool
  Or :: forall s. Expr s Bool -> Expr s Bool -> Expr s Bool

  Call :: forall s a b. Name s (a -> b) -> Expr s a -> Expr s b

  Var :: forall s a. Repr a => Name s a -> Expr s a
  Deref :: forall s a. Repr a => Expr s (Ptr a) -> Expr s a
  Index :: forall sA sB a. (Repr a, (sB :<= sA) ~ True) => Expr sA (Ptr a) -> Expr sB Int -> Expr sA a

var :: forall s a. Repr a => Name s a -> Expr s a
var = Var

(==?), (<?), (>?) :: forall s a. (Repr a, Ord a) => Expr s a -> Expr s a -> Expr s Bool
(==?) = Eql
(<?) = Lt
(>?) = Gt

(&&?), (||?) :: forall s. Expr s Bool -> Expr s Bool -> Expr s Bool
(&&?) = And
(||?) = Or



instance (Num a, Repr a) => Num (Expr s a) where
  (+) = Add
  (*) = Mul
  (-) = Sub
  fromInteger = Literal . fromInteger

