module Security.Util where

import           Data.List

unlines' :: [String] -> String
unlines' = concat . intersperse "\n"

unlines'With :: String -> [String] -> String
unlines'With "" xs = unlines' xs
unlines'With r xs = unlines' (xs ++ [r])

