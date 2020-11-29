{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Security.Analysis.Tree where

import           Security.Util

import           Control.Monad.Writer

import           Data.List

data Tree a   = Node a (Forest a)
  deriving (Functor, Eq, Ord, Show)

type Forest a = [Tree a]

singleton :: a -> Tree a
singleton x = Node x []

addChild' :: Eq a => a -> Tree a -> Tree a -> Writer Progress (Tree a)
addChild' parent child orig@(Node n xs)
  | n == parent = tell Progress >> return (Node n (union [child] xs))
  | otherwise   = return orig

addChild :: Eq a => a -> Tree a -> Tree a -> Tree a
addChild parent child t =
  fst $ runWriter (addChild' parent child t)

newTree :: Eq a => a -> Forest a -> Forest a
newTree x [] = [singleton x]
newTree x orig@(origNode@(Node x' _):rest)
  | x == x' = orig
  | otherwise = origNode : newTree x rest

forestAddChild' :: Eq a => a -> Tree a -> Forest a -> Writer Progress (Forest a)
forestAddChild' parent child = mapM (addChild' parent child)

forestAddChild :: Eq a => a -> Tree a -> Forest a -> Forest a
forestAddChild parent child = map (addChild parent child)

forestAddChildren :: Eq a => a -> [Tree a] -> Forest a -> Forest a
forestAddChildren parent children forest =
  fst $ runWriter $ forestAddChildren' parent children forest

forestAddChildren' :: Eq a => a -> [Tree a] -> Forest a -> Writer Progress (Forest a)
forestAddChildren' parent children forest = foldM (\acc x -> forestAddChild' parent x acc) forest children

unionForests :: Eq a => Forest a -> Forest a -> Forest a
unionForests [] forest = forest
unionForests forest [] = forest
unionForests (Node x xs:rest) forest =
  let (forest', w) = runWriter $ forestAddChildren' x xs forest
  in
    if madeProgress w
      then unionForests rest forest'
      else [Node x xs] `union` unionForests rest forest
      -- else Node x xs : unionForests rest forest

pruneWhenLeavesAre :: (a -> Bool) -> Forest a -> Forest a
pruneWhenLeavesAre _ [] = []
pruneWhenLeavesAre p (t : ts)
  | leavesAre t =     pruneWhenLeavesAre p ts
  | otherwise   = t : pruneWhenLeavesAre p ts
  where
    leavesAre (Node x []) = p x
    leavesAre (Node x xs) = all leavesAre xs

data Progress = NoProgress | Progress

madeProgress :: Progress -> Bool
madeProgress Progress   = True
madeProgress NoProgress = False

instance Semigroup Progress where
  Progress <> _ = Progress
  _ <> Progress = Progress
  _ <> _ = NoProgress

instance Monoid Progress where
  mempty = NoProgress

