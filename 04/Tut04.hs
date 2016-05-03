module Tut04 where

data Tree = Node Int [Tree] deriving Show

noLeaves :: Tree -> Int
noLeaves (Node _ []) = 1
-- noLeaves (Node _ ts) = helper ts
--   where helper :: [Tree] -> Int
--         helper [] = 0
--         helper (t:ts) = noLeaves t + helper ts
noLeaves (Node _ ts) = sum (map noLeaves ts)


-- Die Fkt. even ist bereits definiert, darum nennen wir sie even'
even' :: Tree -> Bool
even' (Node _ []) = True
even' (Node _ ts)
  | even (length ts) = allEven ts
  | otherwise = False
  where
    allEven [] = True
    allEven (t:ts) = even' t && allEven ts

-- Die obligatorische Higher-Order-Variante.
-- all p xs ist genau dann True, wenn alle Elemente von xs das Praedikat p erfuellen.
even'' (Node _ ts) = even (length ts) && all even'' ts
