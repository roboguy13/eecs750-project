module Security.Analysis
  where

import           Security.Analysis.Tree
import qualified Security.Analysis.Tree as Tree
import           Security.Types
import           Security.CodeGen.Types

mkLeakForest :: Cmd a -> Forest SomeName
mkLeakForest c =
  case view c of
    Return _ -> mempty
    AllocSecret size :>>= k -> Tree.singleton 

