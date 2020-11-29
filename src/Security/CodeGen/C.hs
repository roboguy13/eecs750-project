{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wall #-}

module Security.CodeGen.C
  where

import           Security.Repr
import           Security.Types
import           Security.Sensitivity
import           Security.CodeGen.Types
import           Security.Sing
import           Security.CodeGen
import           Security.Util

import           Control.Monad
import           Control.Monad.Operational
import           Data.List

genC :: forall a. Repr a => Cmd a -> CodeGen String
genC = genCNamed <=< genNames

genCNamed :: forall a. Repr a => NamedCmd a -> CodeGen String
genCNamed = genC0 True

genC0 :: forall a. Repr a => Bool -> NamedCmd a -> CodeGen String
genC0 withSemi c =
  case viewCmd0 c of
    Return x -> return "" -- Commands cannot have values in this subset of C

    AllocSecret name size :>>= (k :: Expr s b -> _) -> do
      let d = genDecl ctype name
          alloc = "malloc(" <> show size <> " * sizeof(" <> ptrTypeRepr (ctype @b) <> "))"
      r <- genCNamed (mkCmd0 (k (Var name)))

      return $ unlines'With r [stmt' withSemi [d, "=", alloc]]

    AllocPublic name size :>>= (k :: Expr s b -> _) -> do
      let d = genDecl ctype name
          alloc = "malloc(" <> show size <> " * sizeof(" <> ptrTypeRepr (ctype @b) <> "))"
      r <- genCNamed (mkCmd0 (k (Var name)))
      return $ unlines'With r [stmt' withSemi [d, "=", alloc]]

    Decl name x :>>= k -> do
      let d = genDecl ctype name
          vName = Var name
      xCode <- genExprC (Literal @Public x)
      r <- genCNamed (mkCmd0 (k vName))
      return $ unlines'With r [stmt' withSemi [d, "=", xCode]]

    Assign (Var n) e :>>= k -> do
      eCode <- genExprC e
      r <- genCNamed (mkCmd0 (k ()))
      return $ unlines'With r [stmt' withSemi [emitName n, "=", eCode]]

    -- Assign x@(Index _ _) y :>>= k -> do
    Assign x y :>>= k -> do
      xCode <- genExprC x
      yCode <- genExprC y
      r <- genCNamed (mkCmd0 (k ()))
      return $ unlines'With r [stmt' withSemi [xCode, "=", yCode]]

    IfThenElse cond t f :>>= k -> do
      condCode <- genExprC cond
      tCode <- genCNamed t
      fCode <- genCNamed f

      r <- genCNamed (mkCmd0 (k ()))

      return $ unlines'With r
        ["if (" ++ condCode ++ ") {"
        ,block tCode
        ,"} else {"
        ,block fCode
        ,"}"
        ]

    While cond body :>>= k -> do
      condCode <- genExprC cond
      bodyCode <- genCNamed body
      r <- genCNamed (mkCmd0 (k ()))

      return $ unlines'With r
        ["while (" ++ condCode ++ ") {"
        ,block bodyCode
        ,"}"
        ]

    For loopVar (init :: Expr s c) loopTriple :>>= k -> do
      let sensSing = exprSens init
      -- loopVar <- withSing sensSing freshName
      let loopExpr = withSing sensSing Var loopVar

      let (cond, update, body) = loopTriple () --loopExpr

      initCode <- genExprC init
      condCode <- genExprC cond
      updateCode <- genC0 False update
      bodyCode <- genCNamed body

      r <- genCNamed (mkCmd0 (k ()))

      return $ unlines'With r
        ["for (" ++ genDecl ctype loopVar ++ " = " ++ initCode ++ "; " ++ condCode ++ "; " ++ updateCode ++ ") {"
        ,block bodyCode
        ,"}"
        ]

data Parens = WithParens | NoParens


genExprC :: forall s a. Expr s a -> CodeGen String
genExprC = go NoParens
  where
    genBinOp :: forall s a. Parens -> Expr s a -> Expr s a -> String -> CodeGen String
    genBinOp parens x y op = do
      xCode <- go WithParens x
      yCode <- go WithParens y
      return . parenthesize parens $ unwords [xCode, op, yCode]

    go :: forall s a. Parens -> Expr s a -> CodeGen String
    go _      (Literal x) = return $ lit x
    go parens (Var n)     = return $ emitName n
    go parens (Add x y)   = genBinOp parens x y "+"
    go parens (Sub x y)   = genBinOp parens x y "-"
    go parens (Mul x y)   = genBinOp parens x y "*"
    go parens (Eql x y)   = genBinOp parens x y "=="
    go parens (Lt x y)    = genBinOp parens x y "<"
    go parens (Gt x y)    = genBinOp parens x y ">"
    go parens (Not x)     = do
      xCode <- go WithParens x
      return $ parenthesize parens ('!' : xCode)
    go parens (And x y)   = genBinOp parens x y "&&"
    go parens (Or  x y)   = genBinOp parens x y "||"

    go parens (Call f x)  = do
      fCode <- go WithParens f
      xCode <- go NoParens x
      return $ fCode ++ "(" ++ xCode ++ ")"

    go parens (Deref x) = do
      xCode <- go WithParens x
      return $ parenthesize parens ('*' : xCode)

    go parens (Index ptr ix) = do
      ptrCode <- go WithParens ptr
      ixCode <- go NoParens ix
      return $ parenthesize parens (ptrCode ++ "[" ++ ixCode ++ "]")

parenthesize :: Parens -> String -> String
parenthesize WithParens s = '(' : s ++ ")"
parenthesize NoParens   s = s

stmt' :: Bool -> [String] -> String
stmt' False = unwords
stmt' True  = stmt

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
  ifThenElse (x <? 3)
    (y .= 5)
    (y .= 7)

example4 :: Cmd ()
example4 = do
  x <- decl (21 :: Int)
  while ((1 + 1) <? x)
    (x -= 1)

example5 :: Cmd ()
example5 = do
  x <- decl (1 :: Int)
  for (0 :: Expr Public Int) (\i -> ((i <? 10), (i += 1),
    x += 1))

example6 :: Cmd ()
example6 = do
  arrayA <- allocSecret @Int 8
  arrayB <- allocSecret @Int 8

  for (0 :: Expr Secret Int) (\i -> ((i <? 8), (i += 1), do
    (arrayA ! i) .= (arrayB ! i)
    ))

-- example7 :: Cmd ()
-- example7 = do
--   arrayA <- allocPublic @Int 8
--   arrayB <- allocSecret @Int 8
--   for (0 :: Expr Secret Int) (\i -> ((i <? 8), (i += 1), do
--     (arrayA ! i) .= (arrayB ! i)
--     ))

example8 :: Cmd ()
example8 = do
  secretArray <- allocSecret @Int 8
  publicArray <- allocPublic @Int 8

  for (0 :: Expr Public Int) (\i -> ((i <? 8), (i += 1), do

    leak <- decl (0 :: Int)

    for (0 :: Expr Secret Int) (\j -> ((j <? (secretArray ! i)), (j += 1), do
      leak += 1
      ))
    (publicArray ! i) .= leak
    ))

example9 :: Cmd ()
example9 = do
  secretArrayA <- allocSecret @Int 8
  secretArrayB <- allocSecret @Int 8
  publicArray <- allocPublic @Int 8

  secretArrayB .= secretArrayA

  for (0 :: Expr Public Int) (\i -> ((i <? 8), (i += 1), do

    leak <- decl (0 :: Int)

    for (0 :: Expr Secret Int) (\j -> ((j <? (secretArrayB ! i)), (j += 1), do
      leak += 1
      ))
    (publicArray ! i) .= leak
    ))

