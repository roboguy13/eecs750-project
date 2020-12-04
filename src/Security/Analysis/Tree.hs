{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Security.Analysis.Tree where

import           Security.Util

import           Control.Monad.Writer

import           Data.List
import qualified Data.Set as Set

import Debug.Trace

data Tree a   = Node a (Forest a)
  deriving (Functor, Eq, Ord, Show)

type Forest a = [Tree a]

singleton :: a -> Tree a
singleton x = Node x []

singletonForest :: a -> Forest a
singletonForest = (:[]) . singleton

insertTree :: (Ord a, Eq a) => Forest a -> Tree a -> Forest a
insertTree [] t = [t]
insertTree forest0 t0 = go True forest0 t0
  where
    go False [] t = [t]
    go True  [] _ = []
    go isEq origForest@(Node n cs:restForest) t@(Node n' cs')
      | n' < n  = t : origForest
      | n' > n  = Node n cs : go False restForest t
      | n' == n = Node n cs : go isEq  restForest t


addChildTo :: (Ord a, Eq a) => Forest a -> a -> Tree a -> Forest a
addChildTo [] _parent _child = []
addChildTo (Node n cs:restForest) parent child
  | n == parent = Node n (insertTree cs child) : restForest

  | otherwise   = Node n cs : addChildTo restForest parent child


addChildrenTo :: (Ord a, Eq a) => Forest a -> a -> [Tree a] -> Forest a
addChildrenTo forest parent children = foldr (\child acc -> addChildTo acc parent child) forest children

unionForests :: (Ord a, Eq a) => Forest a -> Forest a -> Forest a
unionForests = fastUnion --foldr (flip insertTree)

fastUnion :: Ord a => [a] -> [a] -> [a]
fastUnion xs ys = Set.toList (Set.fromList (xs ++ ys))

