{-# LANGUAGE DeriveFunctor #-}

module Security.Analysis.Tree where

data Tree a   = Node a (Forest a)
  deriving (Functor)

type Forest a = [Tree a]

singleton :: a -> Tree a
singleton x = Node x []

addChild :: Eq a => a -> a -> Tree a -> Tree a
addChild parent child orig@(Node n xs)
  | n == parent = Node n (Node child []:xs)
  | otherwise   = orig

forestAddChild :: Eq a => a -> a -> Forest a -> Forest a
forestAddChild parent child = map (addChild parent child)

