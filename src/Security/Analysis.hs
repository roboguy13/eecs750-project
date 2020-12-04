{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
import           Control.Arrow
import           Data.List
import qualified Data.Set as Set

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
  | isSecretName sn = newTree (Tree.singleton sn) forest
  | otherwise       = forest

consIf :: (a -> Bool) -> a -> [a] -> [a]
consIf p x xs
  | p x = x : xs
  | otherwise = xs

onHead :: (a -> a) -> [a] -> [a]
onHead f [] = []
onHead f (x:xs) = f x : xs

-- -- | The 'Sensitivity' here depends on whether the scope depends on
-- -- a secret variable in a condition. It starts out as 'Public' and gets
-- -- updated as necessary.

type Scope =
  [([SomeName]  -- | Secret variables from enclosing conditional
  , [SomeName]) -- | Public variables on the from the enclosing scope
  ]

emptyScope :: Scope
emptyScope = []

scopeAddName :: Scope -> SomeName -> Scope
scopeAddName scope newName =
  onHead (\(secretDeps, sns) -> (secretDeps, newName : sns)) scope

scopeSetSecretDeps :: Scope -> [SomeName] -> Scope
scopeSetSecretDeps [] secretDeps = []
scopeSetSecretDeps ((_, sns):scope) secretDeps = (secretDeps, sns) : scope

scopePush :: Scope -> [SomeName] -> Scope
scopePush scope secretDeps = ([], []) : scopeSetSecretDeps scope secretDeps

scopePop :: Scope -> Scope
scopePop = drop 1

-- secretInLocalScope :: Scope -> SomeName -> Bool
-- secretInLocalScope [] sn = False
-- secretInLocalScope (locals:_) sn = sn `elem` (map snd locals) && isSecretName sn

-- publicNonLocal = undefined

publicNonLocal :: Scope -> SomeName -> Maybe [SomeName]
publicNonLocal scope sn =
  let enclosingScope = scopePop scope
  in
    fmap (fastNub . fst) $ find ((sn `elem`) . snd) enclosingScope


-- isInLocalScope :: Scope -> SomeName -> Bool
-- isInLocalScope [] name = False
-- isInLocalScope ((_, ns):_) name = name `elem` ns

-- isInSecretScope :: Scope -> SomeName -> Bool
-- isInSecretScope scope name = any (name `elem`) . map snd $ filter ((== Secret) . fst) scope

-- publicOutOfLocalScope :: Scope -> SomeName -> Bool
-- publicOutOfLocalScope scope0 name =
--   let scope = scopePop scope0
--   in

strength :: Functor f => (f a, b) -> f (a, b)
strength (fa, b) = fmap (\a -> (a, b)) fa

mkLeakForest :: NamedCmd a -> Forest SomeName
-- mkLeakForest = {- pruneWhenLeavesAre isSecretName . -} go [] [] []
mkLeakForest = fastNub . go emptyScope []
  where
    go :: Scope -> Forest SomeName -> NamedCmd ty -> Forest SomeName
    go scope forest c =
      case viewCmd0 c of
        Return _ -> forest

        AllocSecret name size :>>= k ->
          let sn = mkSomeName name
          in
          go (scopeAddName scope sn) (insertTreeIfSecret forest sn) (mkCmd0 (k (Var name)))

        AllocPublic name size :>>= k ->
          let sn = mkSomeName name
          in
          go (scopeAddName scope sn) forest (mkCmd0 (k (Var name)))

        Decl name x :>>= k ->
          let sn = mkSomeName name
          in
          go (scopeAddName scope sn) forest (mkCmd0 (k (Var name)))

        Assign lhs rhs :>>= k ->
          let lhsSns = fastNub $ collectSomeNames lhs
              rhsSns = fastNub $ collectSomeNames rhs
              -- forest' =
              --   foldr (\rhsSn -> forestAddChildren rhsSn (map Tree.singleton rhsSns))
              --         forest
              --         (filter isSecretName lhsSns)
              forest' =
                foldr (\rhsSn -> forestAddChildren rhsSn (map Tree.singleton lhsSns))
                      forest
                      (filter isSecretName rhsSns)
          in
          case strength (mapM (publicNonLocal scope &&& id) lhsSns) of
            Just (secretDeps, lhsSn) ->
              let forest'' =
                    foldr (\p -> forestAddChildren p (map Tree.singleton lhsSns))
                          forest'
                          (fastNub secretDeps)
              in
              go scope forest'' (mkCmd0 (k ()))
            Nothing ->
              go scope forest' (mkCmd0 (k ()))

        IfThenElse cond t f :>>= k ->
          let secretDeps = fastNub $ filter isSecretName $ collectSomeNames cond
              tForest = go (scopePush scope secretDeps) forest t
              fForest = go (scopePush scope secretDeps) forest f
          in
          go scope (tForest `unionForests` fForest) (mkCmd0 (k ()))

        While cond body :>>= k ->
          let secretDeps = fastNub $ filter isSecretName $ collectSomeNames cond
              bodyForest = go (scopePush scope secretDeps) forest body
          in
          go scope bodyForest (mkCmd0 (k ()))

        For loopVar (init :: Expr s c) loopTriple :>>= k ->
          let loopSn = mkSomeName loopVar
              (cond, update, body) = loopTriple ()
              secretDeps = fastNub $ filter isSecretName $ collectSomeNames cond

              updateForest = go (scopePush scope secretDeps) forest update
              bodyForest = go (scopePush scope secretDeps) forest body
          in
          go scope (updateForest `unionForests` bodyForest) (mkCmd0 (k ()))

