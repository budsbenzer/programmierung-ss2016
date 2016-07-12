module Tut12 where

import Prelude hiding (unzip)

unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([], [])
-- unzip ((a,b):xs) = (a : fst (unzip xs),
--                     b : snd (unzip xs))
unzip ((a,b):zs) = (a:ls, b:rs)
  where (ls, rs) = unzip zs

data Tree = Null | Node Int Tree Tree deriving (Show)

isPatternOf Null _ = True
isPatternOf _ Null = False
isPatternOf t (Node x l r) = isPrefixOf t (Node x l r)
                             || isPatternOf t l
                             || isPatternOf t r

isPrefixOf :: Tree -> Tree -> Bool
isPrefixOf Null _ = True
isPrefixOf _ Null = False
isPrefixOf (Node x l r) (Node u v w) = isPrefixOf l v && isPrefixOf r w && x == u
