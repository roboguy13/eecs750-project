module Security.Analysis.DOT
  (genDOT)
  where

import           Security.Analysis.Tree
import           Security.Util
import           Security.Sensitivity
import           Security.CodeGen.Types
import           Data.List


-- | Generate a DOT format representation
genDOT :: Forest SomeName -> String
genDOT forest =
  unlines
    ["digraph root {"
    , unlines' (zipWith genTreeDOT [1..] forest)
    , "}"
    ]

genTreeDOT :: Int -> Tree SomeName -> String
genTreeDOT n t =
  unlines'
    ["subgraph " <> show n <> " {"
    ,unlines' $ preprocess $ getNameList t
    ,unlines' (go t)
    ,"}"
    ]
  where
    showNode (Node x _) = emitSomeName x

    go :: Tree SomeName -> [String]
    go (Node x []) = []
    go (Node x ys) =
      map (\y -> "  " ++ emitSomeName x ++ " -> " ++ showNode y ++ ";") ys
        ++ concatMap go ys

    getNameList :: Tree SomeName -> [SomeName]
    getNameList (Node x xs) = [x] `union` concatMap getNameList xs

    preprocess :: [SomeName] -> [String]
    preprocess [] = []
    preprocess (n:ns) =
      let def = case someNameSens n of
                  Secret -> Just $ "  " <> emitSomeName n <> " [shape=box];"
                  Public -> Nothing
      in
      consMaybe def (preprocess ns)

consMaybe :: Maybe a -> [a] -> [a]
consMaybe Nothing xs = xs
consMaybe (Just x) xs = x:xs

