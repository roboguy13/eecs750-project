{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Security.CodeGen
  where

import           Security.Types
import           Security.Sensitivity
import           Security.Sing
import           Security.CodeGen.Types

import           Control.Monad.Operational

genNames :: Cmd a -> CodeGen (NamedCmd a)
genNames c =
  case view c of
    Return x -> return $ return x

    AllocSecret NoName size :>>= k -> do
      name <- freshName @Secret
      let var = Var name
          r = k var
      r' <- genNames r

      return (singleton (AllocSecret name size) >> r')

    AllocPublic NoName size :>>= k -> do
      name <- freshName @Public
      let var = Var name
          r = k var
      r' <- genNames r

      return (singleton (AllocPublic name size) >> r')

    Decl NoName x :>>= k -> do
      name <- freshName @Public

      let var = Var name
          r = k var
      r' <- genNames r

      return (singleton (Decl name x) >> r')

    Assign x y :>>= k -> do
      r' <- genNames (k ())
      return (singleton (Assign x y) >> r')

    IfThenElse cond t f :>>= k -> do
      r' <- genNames (k ())
      t' <- genNames t
      f' <- genNames f
      return (singleton (IfThenElse cond t' f') >> r')

    While cond body :>>= k -> do
      r' <- genNames (k ())
      body' <- genNames body
      return (singleton (While cond body') >> r')

    For NoName (init :: Expr s c) loopTriple :>>= k -> do
      let sensSing = exprSens init
      loopVar <- withSing sensSing freshName
      let loopExpr = withSing sensSing Var loopVar

      let (cond, update, body) = loopTriple loopExpr

      update' <- genNames update
      body' <- genNames body

      r' <- genNames (k ())

      return (singleton (For loopVar init (\() -> (cond, update', body'))) >> r')

