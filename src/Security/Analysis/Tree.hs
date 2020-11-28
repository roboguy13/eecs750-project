{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Security.Analysis.Tree where

import           Security.Util

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

-- | Generate a DOT format representation
genDOT :: (a -> String) -> Forest a -> String
genDOT showVal forest = unlines' (map (genTreeDOT showVal) forest)

genTreeDOT :: forall a. (a -> String) -> Tree a -> String
genTreeDOT showVal t =
  unlines'
    ["digraph {"
    ,unlines' (go t)
    ,"}"
    ]
  where
    showNode (Node x _) = showVal x

    go :: Tree a -> [String]
    go (Node x []) = []
    go (Node x ys) =
      map (\y -> "  " ++ showVal x ++ " -> " ++ showNode y ++ ";") ys
        ++ concatMap go ys

