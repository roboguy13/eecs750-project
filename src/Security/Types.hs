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
data CmdF (name :: Sensitivity -> Type -> Type) a where
  AllocSecret :: forall a name. Repr a => name Secret (Ptr a) -> Int -> CmdF name (Expr Secret (Ptr a))
  AllocPublic :: forall a name. Repr a => name Public (Ptr a) -> Int -> CmdF name (Expr Public (Ptr a))

  Decl :: forall a name. Repr a => name Public a -> a -> CmdF name (Expr Public a)
  Assign :: forall s a name. Repr a => Expr s a -> Expr s a -> CmdF name ()  -- | memcpy's arrays
  -- Memcpy :: forall s a. Expr s (Array a) -> Expr s (Array a) ->

  NameFFI :: forall a name. String -> CmdF name a

  IfThenElse :: forall s a name. Repr a => Expr s Bool -> Cmd0 name a -> Cmd0 name a -> CmdF name ()
  While :: forall s a name. Repr a => Expr s Bool -> Cmd0 name a -> CmdF name ()
  For :: forall s a name. (Repr a) => name s a -> Expr s a -> (NameArg name s a -> (Expr s Bool, Cmd0 name (), Cmd0 name ())) -> CmdF name ()

data NoName (s :: Sensitivity) a = NoName

-- TODO: Figure out if there is a more straightforward way to do this
-- (maybe without a type family like this one)
type family NameArg a (s :: Sensitivity) t where
  NameArg NoName s t = Expr s t
  NameArg Name   s t = ()

type Cmd0 name = Program (CmdF name)

type Cmd = Program (CmdF NoName)

type NamedCmd = Program (CmdF Name)

allocSecret :: forall a. Repr a => Int -> Cmd (Expr Secret (Ptr a))
allocSecret = singleton . AllocSecret @a NoName

allocPublic :: forall a. Repr a => Int -> Cmd (Expr Public (Ptr a))
allocPublic = singleton . AllocPublic @a NoName

decl :: forall a. Repr a => a -> Cmd (Expr Public a)
decl = singleton . Decl @a NoName

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
for initial loopTriple = singleton (For NoName initial loopTriple)


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

