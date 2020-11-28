{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Security.Types
  where

import           GHC.Types (Type)
import           Control.Monad.Operational
import           Security.Repr
import           Security.Sensitivity
import           Security.CodeGen.Types
import           Security.Sing


-- TODO: Add a mechanism for "Secret" lambdas?
--
-- TODO: Add another type parameter to represent names, which can "start
-- out" as 'Void' and then a function fills in the names (annotating the
-- AST with names) to be used by the code generator and the analysis tool?
data CmdF a where
  AllocSecret :: forall a. Repr a => Int -> CmdF (Expr Secret (Ptr a))
  AllocPublic :: forall a. Repr a => Int -> CmdF (Expr Public (Ptr a))

  Decl :: forall a. Repr a => a -> CmdF (Expr Public a)
  Assign :: forall s a. Repr a => Expr s a -> Expr s a -> CmdF ()  -- | memcpy's arrays
  -- Memcpy :: forall s a. Expr s (Array a) -> Expr s (Array a) ->

  NameFFI :: forall a. String -> CmdF a

  IfThenElse :: forall s a. Repr a => Expr s Bool -> Cmd a -> Cmd a -> CmdF ()
  While :: forall s a. Repr a => Expr s Bool -> Cmd a -> CmdF ()
  For :: forall s a. (Repr a) => Expr s a -> (Expr s a -> (Expr s Bool, Cmd (), Cmd ())) -> CmdF ()

type Cmd = Program CmdF

allocSecret :: forall a. Repr a => Int -> Cmd (Expr Secret (Ptr a))
allocSecret = singleton . AllocSecret @a

allocPublic :: forall a. Repr a => Int -> Cmd (Expr Public (Ptr a))
allocPublic = singleton . AllocPublic @a

decl :: forall a. Repr a => a -> Cmd (Expr Public a)
decl = singleton . Decl @a

infixr 0 .=
(.=) :: forall s a. Repr a => Expr s a -> Expr s a -> Cmd ()
x .= y = singleton (Assign x y)

nameFFI :: forall a. String -> Cmd a
nameFFI = singleton . NameFFI @a

ifThenElse :: forall s a. Repr a => Expr s Bool -> Cmd a -> Cmd a -> Cmd ()
ifThenElse c t f = singleton (IfThenElse c t f)

while :: forall s a. Repr a => Expr s Bool -> Cmd a -> Cmd ()
while c b = singleton (While c b)

for :: forall s a. (Repr a) => Expr s a -> (Expr s a -> (Expr s Bool, Cmd (), Cmd ())) -> Cmd ()
for initial loopTriple = singleton (For initial loopTriple)


-- type LVal s a = forall side. Expr side s a

(+=) :: forall s a. (SingI s, Repr a, Num a) => Expr s a -> Expr s a -> Cmd ()
x += y = x .= x + y

(-=) :: forall s a. (SingI s, Repr a, Num a) => Expr s a -> Expr s a -> Cmd ()
x -= y = x .= x - y

test :: Cmd ()
test = do
  x <- decl (1 :: Int)
  let y = x + x

  x .= y

  return ()


data Expr  (s :: Sensitivity) a where
  Literal :: forall s a. (SingI s, Repr a) => a -> Expr s a
  -- LValExpr :: forall s a. LVal s a -> Expr s a

  Add :: forall s a. (Repr a, Num a) => Expr s a -> Expr s a -> Expr s a
  Sub :: forall s a. (Repr a, Num a) => Expr s a -> Expr s a -> Expr s a
  Mul :: forall s a. (Repr a, Num a) => Expr s a -> Expr s a -> Expr s a

  Eql :: forall s a. (Repr a, Eq a) => Expr s a -> Expr s a -> Expr s Bool
  Lt :: forall s a. (Repr a, Ord a) => Expr s a -> Expr s a -> Expr s Bool
  Gt :: forall s a. (Repr a, Ord a) => Expr s a -> Expr s a -> Expr s Bool

  T :: forall s. SingI s => Expr s Bool
  F :: forall s. SingI s => Expr s Bool
  Not :: forall s. Expr s Bool -> Expr s Bool
  And :: forall s. Expr s Bool -> Expr s Bool -> Expr s Bool
  Or :: forall s. Expr s Bool -> Expr s Bool -> Expr s Bool

  Call :: forall s a b.  Expr s (a -> b) -> Expr s a -> Expr s b

  Var :: forall s a. (SingI s, Repr a) => Name s a -> Expr s a
  Deref :: forall s a. Repr a => Expr s (Ptr a) -> Expr s a
  Index :: forall sA sB a. (Repr a, (sB :<= sA) ~ True) => Expr sA (Ptr a) -> Expr sB Int -> Expr sA a

var :: forall s a. (SingI s, Repr a) => Name s a -> Expr s a
var = Var

(==?), (<?), (>?) :: forall s a. (Repr a, Ord a) => Expr s a -> Expr s a -> Expr s Bool
(==?) = Eql
(<?) = Lt
(>?) = Gt

(&&?), (||?) :: forall s. Expr s Bool -> Expr s Bool -> Expr s Bool
(&&?) = And
(||?) = Or

(!) :: forall sA sB a. (Repr a, (sB :<= sA) ~ True) => Expr sA (Ptr a) -> Expr sB Int -> Expr sA a
(!) = Index


instance (SingI s, Num a, Repr a) => Num (Expr s a) where
  (+) = Add
  (*) = Mul
  (-) = Sub
  fromInteger = Literal . fromInteger

exprSens :: Expr s a -> Sing s
exprSens (Literal _) = sing
exprSens (Add x _) = exprSens x
exprSens (Sub x _) = exprSens x
exprSens (Mul x _) = exprSens x
exprSens (Eql x _) = exprSens x
exprSens (Lt x _) = exprSens x
exprSens (Gt x _) = exprSens x
exprSens T = sing
exprSens F = sing
exprSens (Not x) = exprSens x
exprSens (And x _) = exprSens x
exprSens (Or x _) = exprSens x
exprSens (Call f _) = exprSens f
exprSens (Var _) = sing
exprSens (Deref x) = exprSens x
exprSens (Index x _) = exprSens x

