{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Security.CodeGen.C
  where

import           Security.Repr
import           Security.Types
import           Security.Sensitivity
import           Security.CodeGen.Types

import           Control.Monad.Operational

genC :: forall a. Repr a => Cmd a -> CodeGen String
genC c =
  case view c of
    AllocSecret size :>>= k -> do
      name <- freshName @Secret
      let d = genDecl ctype name
      r <- genC (k (Var name))
      return $ unlines [stmt [d], r]

    AllocPublic size :>>= k -> do
      name <- freshName @Public
      let d = genDecl ctype name
      r <- genC (k (Var name))
      return $ unlines [stmt [d], r]

    Decl x :>>= k -> do
      name <- freshName @Public
      let d = genDecl ctype name
          vName = Var name
      assignment <- genC (vName .= Literal x)
      r <- genC (k name)
      return $ unlines [stmt [d], assignment, r]

    Assign (Var n) e :>>= k -> do
      eCode <- genExprC e
      return $ unlines [stmt [emitName n, "=", eCode]]

    IfThenElse cond t f :>>= k -> do
      condCode <- genExprC cond
      tCode <- genC t
      fCode <- genC f

      return $ unlines
        ["if (" ++ condCode ++ ") {"
        ,tCode
        ,"} else {"
        ,fCode
        ,"}"
        ]

genExprC :: forall s a. Repr a => Expr s a -> CodeGen String
genExprC (Literal x) = return $ lit x
genExprC (Var n) = return $ emitName n
genExprC (Add x y) = do
  xCode <- genExprC x
  yCode <- genExprC y
  return $ unwords [xCode, "+", yCode]



example1 :: Cmd ()
example1 = do
  array1 <- allocSecret @Int 8
  array2 <- allocSecret @Int 8

  array2 .= array1


-- example2 :: Cmd ()
-- example2 = do
--   array1 <- allocSecret @Int 8
--   array2 <- allocPublic @Int 8

--   array2 .= array1

