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
insertTree origForest@(Node n cs:restForest) t@(Node n' cs')
  | n' < n  = t : origForest
  | n' >= n = Node n cs : insertTree restForest t


addChildTo :: (Ord a, Eq a) => Forest a -> a -> Tree a -> Forest a
addChildTo [] _parent _child = []
addChildTo (Node n cs:restForest) parent child
  | n == parent = Node n (insertTree cs child) : restForest

  | otherwise   = Node n cs : addChildTo restForest parent child

addChildrenTo :: (Ord a, Eq a) => Forest a -> a -> [Tree a] -> Forest a
addChildrenTo forest parent children = foldr (\child acc -> addChildTo acc parent child) forest children

unionForests :: (Ord a, Eq a) => Forest a -> Forest a -> Forest a
unionForests = foldr (flip insertTree)

-- -- addChild' :: Eq a => a -> Tree a -> Tree a -> Writer Progress (Tree a)
-- -- addChild' parent child orig@(Node n []) =
-- --   | n == parent = tell Progress >> return (Node n [child])
-- --   | otherwise   = return orig
-- -- addChild' parent child@(Node childN childCs) orig@(Node n (c:cs))
-- --   | n == parent =
-- --       if 

-- addChild' :: Eq a => a -> Tree a -> Tree a -> Writer Progress (Tree a)
-- addChild' parent child orig@(Node n xs)
--   | n == parent = tell Progress >> return (Node n (union [child] xs))
--   -- | n == parent = tell Progress >> return (Node n (unionForests [child] xs))
--   | otherwise   = Node <$> pure n <*> mapM (addChild' parent child) xs

-- addChild :: Eq a => a -> Tree a -> Tree a -> Tree a
-- addChild parent child t =
--   fst $ runWriter (addChild' parent child t)

-- newTreeEither :: (Ord a, Eq a, Show a) => Tree a -> Forest a -> Either (Maybe (Forest a)) (Forest a)
-- newTreeEither t [] = Right [t]
-- newTreeEither t@(Node x xs) (origNode@(Node x' xs'):rest)
--   | x == x' =
--       if xs /= xs'
--         then Left Nothing --Left $ Just (union [Node x (xs `unionForests` xs')] rest)
--         else Left Nothing
--   | otherwise = do
--       case newTreeEither t xs' of
--         Right _ -> do
--           rest' <- newTreeEither t rest
--           return (union [origNode] rest')
--         Left Nothing -> Left Nothing
--         Left (Just forest') ->
--           Left $ Just (forest' `unionForests` rest)
--           -- Left (Node x' forest':rest)

-- nodeItem :: Tree a -> a
-- nodeItem (Node x _) = x

-- newTree :: (Ord a, Eq a, Show a) => Tree a -> Forest a -> Forest a
-- newTree x t =
--   case newTreeEither x t of
--     Right t' -> traceShow ("newTree: " ++ show t') t'
--     Left Nothing -> t
--     Left (Just t') -> traceShow ("newTree 2: " ++ show t') t'

-- forestAddChild' :: (Ord a, Eq a) => a -> Tree a -> Forest a -> Writer Progress (Forest a)
-- forestAddChild' parent child = mapM (addChild' parent child)

-- forestAddChild :: (Ord a, Eq a) => a -> Tree a -> Forest a -> Forest a
-- forestAddChild parent child = map (addChild parent child)

-- forestAddChildren :: (Ord a, Eq a) => a -> [Tree a] -> Forest a -> Forest a
-- forestAddChildren parent children forest =
--   fst $ runWriter $ forestAddChildren' parent children forest

-- forestAddChildren' :: (Ord a, Eq a) => a -> [Tree a] -> Forest a -> Writer Progress (Forest a)
-- forestAddChildren' parent children forest = foldM (\acc x -> fastNub <$> forestAddChild' parent x acc) forest children

-- unionForests :: (Ord a, Eq a, Show a) => Forest a -> Forest a -> Forest a
-- unionForests [] forest = forest
-- unionForests forest [] = forest
-- unionForests (Node x xs:rest) forest =
--   let (forest', w) = runWriter $ forestAddChildren' x xs forest
--   in
--     if madeProgress w
--       then unionForests rest forest'
--       else fastNub $ newTree (Node x xs) (unionForests rest forest)
--       -- else [Node x xs] `union` unionForests rest forest
--       -- else Node x xs : unionForests rest forest

-- pruneWhenLeavesAre :: (a -> Bool) -> Forest a -> Forest a
-- pruneWhenLeavesAre _ [] = []
-- pruneWhenLeavesAre p (t : ts)
--   | leavesAre t =     pruneWhenLeavesAre p ts
--   | otherwise   = t : pruneWhenLeavesAre p ts
--   where
--     leavesAre (Node x []) = p x
--     leavesAre (Node x xs) = all leavesAre xs

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

fastNub :: Ord a => [a] -> [a]
fastNub = Set.toList . Set.fromList

