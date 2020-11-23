{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Security.CodeGen.C
  where

import           Security.Repr
import           Security.Types
import           Security.Sensitivity
import           Security.CodeGen.Types

import           Control.Monad.Operational
import           Data.List

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
      xCode <- genExprC (Literal x)
      r <- genC (k name)
      return $ unlines [stmt [d, "=", xCode], r]

    Assign (Var n) e :>>= k -> do
      eCode <- genExprC e
      return $ unlines [stmt [emitName n, "=", eCode]]

    IfThenElse cond t f :>>= k -> do
      condCode <- genExprC cond
      tCode <- genC t
      fCode <- genC f

      return $ unlines
        ["if (" ++ condCode ++ ") {"
        ,block tCode
        ,"} else {"
        ,block fCode
        ,"}"
        ]

    While cond body :>>= k -> do
      condCode <- genExprC cond
      bodyCode <- genC body
      return $ unlines
        ["while (" ++ condCode ++ ") {"
        ,block bodyCode
        ,"}"
        ]

data Parens = WithParens | NoParens


genExprC :: forall s a. Repr a => Expr s a -> CodeGen String
genExprC = go NoParens
  where
    genBinOp :: forall s a. Repr a => Parens -> Expr s a -> Expr s a -> String -> CodeGen String
    genBinOp parens x y op = do
      xCode <- go WithParens x
      yCode <- go WithParens y
      return . withParens parens $ unwords [xCode, op, yCode]

    go _      (Literal x) = return $ lit x
    go parens (Var n)     = return $ emitName n
    go parens (Add x y)   = genBinOp parens x y "+"
    go parens (Sub x y)   = genBinOp parens x y "-"
    go parens (Mul x y)   = genBinOp parens x y "*"
    go parens (Eql x y)   = genBinOp parens x y "=="
    go parens (Lt x y)    = genBinOp parens x y "<"
    go parens (Gt x y)    = genBinOp parens x y ">"

withParens :: Parens -> String -> String
withParens WithParens s = '(' : s ++ ")"
withParens NoParens   s = s

-- parens :: String -> String
-- parens = ('(':) . (++")")

entryPoint :: CodeGen String -> CodeGen String
entryPoint = fmap go
  where
    go code =
      concat $
      intersperse "\n"
        ["int main() {"
        ,block code
        ,"}"
        ]

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

example3 :: Cmd ()
example3 = do
  x <- decl (1 :: Int)
  y <- decl (0 :: Int)
  ifThenElse (Var x <? 3)
    (Var y .= 5)
    (Var y .= 7)

example4 :: Cmd ()
example4 = do
  x <- decl (21 :: Int)
  while ((1 + 1) <? Var x)
    (Var x -= 1)

