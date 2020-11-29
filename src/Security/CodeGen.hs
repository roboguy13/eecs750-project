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
  case viewCmd0 c of
    Return x -> return $ mkCmd0 $ return x

    AllocSecret NoName size :>>= k -> do
      name <- freshName @Secret
      let var = Var name
          r = k var
      r' <- genNames (mkCmd0 r)

      return $ (toCmd0 (AllocSecret name size) `andThen` r')

    AllocPublic NoName size :>>= k -> do
      name <- freshName @Public
      let var = Var name
          r = k var
      r' <- genNames (mkCmd0 r)

      return (toCmd0 (AllocPublic name size) `andThen` r')

    Decl NoName x :>>= k -> do
      name <- freshName @Public

      let var = Var name
          r = k var
      r' <- genNames (mkCmd0 r)

      return (toCmd0 (Decl name x) `andThen` r')

    Assign x y :>>= k -> do
      r' <- genNames (mkCmd0 (k ()))
      return (toCmd0 (Assign x y) `andThen` r')

    IfThenElse cond t f :>>= k -> do
      r' <- genNames (mkCmd0 (k ()))
      t' <- genNames t
      f' <- genNames f
      return (toCmd0 (IfThenElse cond t' f') `andThen` r')

    While cond body :>>= k -> do
      r' <- genNames (mkCmd0 (k ()))
      body' <- genNames body
      return (toCmd0 (While cond body') `andThen` r')

    For NoName (init :: Expr s c) loopTriple :>>= k -> do
      let sensSing = exprSens init
      loopVar <- withSing sensSing freshName
      let loopExpr = withSing sensSing Var loopVar

      let (cond, update, body) = loopTriple loopExpr

      update' <- genNames update
      body' <- genNames body

      r' <- genNames (mkCmd0 (k ()))

      return (toCmd0 (For loopVar init (\() -> (cond, update', body'))) `andThen` r')

