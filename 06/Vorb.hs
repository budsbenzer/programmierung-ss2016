module Vorb where

data UTree a = UNode a [UTree a] deriving (Show, Eq)

data BTree a = BNil | BNode a (BTree a) (BTree a) deriving (Show, Eq)


cns :: UTree a -> BTree a
cns = cns' . return
  where 
    cns' :: [UTree a] -> BTree a
    cns' [] = BNil
    cns' (UNode x cs : ts) = BNode x (cns' cs) (cns' ts)


-- usum :: UTree Int -> Int
-- usum (UNode x ts) = x + lsum ts

-- lsum :: [UTree Int] -> Int
-- lsum [] = 0
-- lsum (t:ts) = usum t + lsum ts

-- bsum BNil = 0
-- bsum (BNode x l r) = x + bsum l + bsum r

