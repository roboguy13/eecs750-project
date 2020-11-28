{-# LANGUAGE ScopedTypeVariables #-}

module Security.Analysis
  where

import           Security.Analysis.Tree
import qualified Security.Analysis.Tree as Tree
import           Security.Types
import           Security.Sensitivity
import           Security.CodeGen.Types

import           Security.CodeGen.C
import           Security.CodeGen
import           Security.Analysis.DOT

import           Control.Monad.Operational
import           Control.Applicative
import           Data.List

-- | Get the names in an Expr
collectSomeNames :: Expr s a -> [SomeName]
collectSomeNames (Var n) = [mkSomeName n]
collectSomeNames (Literal _) = []
collectSomeNames (Add x y) = collectSomeNames x ++ collectSomeNames y
collectSomeNames (Sub x y) = collectSomeNames x ++ collectSomeNames y
collectSomeNames (Mul x y) = collectSomeNames x ++ collectSomeNames y
collectSomeNames (Eql x y) = collectSomeNames x ++ collectSomeNames y
collectSomeNames (Lt x y) = collectSomeNames x ++ collectSomeNames y
collectSomeNames (Gt x y) = collectSomeNames x ++ collectSomeNames y
collectSomeNames T = []
collectSomeNames F = []
collectSomeNames (Not x) = collectSomeNames x
collectSomeNames (And x y) = collectSomeNames x ++ collectSomeNames y
collectSomeNames (Or x y) = collectSomeNames x ++ collectSomeNames y
collectSomeNames (Call f x) = collectSomeNames f ++ collectSomeNames x
collectSomeNames (Deref x) = collectSomeNames x
collectSomeNames (Index arr ix) = collectSomeNames arr ++ collectSomeNames ix

isSecretName :: SomeName -> Bool
isSecretName sn =
  case someNameSens sn of
    Secret -> True
    Public -> False

insertTreeIfSecret :: Forest SomeName -> SomeName -> Forest SomeName
insertTreeIfSecret forest sn
  | isSecretName sn = Tree.singleton sn : forest
  | otherwise       = forest

consIf :: (a -> Bool) -> a -> [a] -> [a]
consIf p x xs
  | p x = x : xs
  | otherwise = xs

mkLeakForest :: NamedCmd a -> Forest SomeName
mkLeakForest = pruneWhenLeavesAre isSecretName . go [] [] []
  where
    go :: [SomeName] -> [SomeName] -> Forest SomeName -> NamedCmd ty -> Forest SomeName
    go secretDeps {- dependencies from an enclosing control structure -} localNames forest c =
      let isNonLocal sn = sn `notElem` localNames
          isSecretOrNonLocal = liftA2 (||) isSecretName isNonLocal
      in
      case view c of
        Return _ -> forest

        AllocSecret name size :>>= k ->
          let sn = mkSomeName name
          in
          go secretDeps (sn : localNames) (insertTreeIfSecret forest sn) (k (Var name))

        AllocPublic name size :>>= k ->
          let sn = mkSomeName name
          in
          go secretDeps (sn : localNames) forest (k (Var name))

        Decl name x :>>= k ->
          let sn = mkSomeName name
          in
          go secretDeps (sn : localNames) (insertTreeIfSecret forest sn) (k (Var name))

        Assign lhs rhs :>>= k ->
          let lhsSns = collectSomeNames lhs
              forest' =
                foldr (\x acc -> forestAddChildren x lhsSns acc) forest (filter isSecretName (collectSomeNames rhs))
                  ++
                foldr (\x acc -> forestAddChildren x (filter isNonLocal lhsSns) acc) forest secretDeps
          in
          go secretDeps localNames forest' (k ())

        IfThenElse cond t f :>>= k ->
          let condSecretNames = filter isSecretName (collectSomeNames cond)
              secretDeps' = condSecretNames ++ secretDeps
              tForest = go secretDeps [] forest t
              fForest = go secretDeps [] forest f
              forest' = union tForest fForest
          in
          go secretDeps localNames forest' (k ())

        While cond body :>>= k ->
          let condSecretNames = filter isSecretName (collectSomeNames cond)
              bodyForest = go condSecretNames [] forest body
          in
          go secretDeps localNames bodyForest (k ())

        For loopVar (init :: Expr s c) loopTriple :>>= k ->
          let loopSn = mkSomeName loopVar
              (cond, update, body) = loopTriple ()
              condSecretNames = filter isSecretName (collectSomeNames cond)
              secretDeps' = condSecretNames ++ secretDeps
              forest' = go secretDeps' [loopSn] forest update `union` go secretDeps' [loopSn] forest body
          in
          go (consIf isSecretName loopSn secretDeps) localNames forest' (k ())

