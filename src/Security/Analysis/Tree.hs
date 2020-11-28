{-# LANGUAGE DeriveFunctor #-}

module Security.Analysis.Tree where

data Tree a   = Node a (Forest a)
  deriving (Functor, Eq, Ord, Show)

type Forest a = [Tree a]

singleton :: a -> Tree a
singleton x = Node x []

addChild :: Eq a => a -> a -> Tree a -> Tree a
addChild parent child orig@(Node n xs)
  | n == parent = Node n (Node child [] : xs)
  | otherwise   = orig

forestAddChild :: Eq a => a -> a -> Forest a -> Forest a
forestAddChild parent child = map (addChild parent child)

forestAddChildren :: Eq a => a -> [a] -> Forest a -> Forest a
forestAddChildren parent children forest = foldr (\x acc -> forestAddChild parent x acc) forest children

pruneWhenLeavesAre :: (a -> Bool) -> Forest a -> Forest a
pruneWhenLeavesAre _ [] = []
pruneWhenLeavesAre p (t : ts)
  | leavesAre t =     pruneWhenLeavesAre p ts
  | otherwise   = t : pruneWhenLeavesAre p ts
  where
    leavesAre (Node x []) = p x
    leavesAre (Node x xs) = all leavesAre xs

