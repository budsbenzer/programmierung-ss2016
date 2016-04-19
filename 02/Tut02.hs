module Tut02 where

import Prelude hiding (words, unwords)

comp :: [Int] -> [Int] -> Bool
comp [] [] = True
comp [] xs = False
comp xs [] = False
comp (x:xs) (y:ys)
  | x == y = comp xs ys
  | otherwise = False
  -- = x == y && comp xs ys

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

data Tree = Leaf Int | Branch Tree Tree

countLeaves :: Tree -> Int
countLeaves (Leaf _) = 1
countLeaves (Branch b1 b2) = countLeaves b1
  + countLeaves b2

yield :: Tree -> [Int]
yield (Leaf i) = [i]
yield (Branch l r) = yield l ++ yield r

-- f :: [Int] -> Int
-- f xs = foldr (*) 1 (map g (filter even xs))  
--   where g x = x * x
-- f xs = foldr (*) 1 (map (^2) (filter even xs))
-- f xs = (foldr (*) 1 . map (^2) . filter even) xs
f = foldr (*) 1 . map (^2) . filter even

unwords :: [String] -> String
unwords [] = ""
unwords [s] = s
unwords (s:ss) = s ++ " " ++ unwords ss

chop :: String -> (String, String)
chop "" = ("", "")
chop (' ':xs) = ("", xs)
-- chop (x:xs) = (x : fst (chop xs), snd (chop xs))
chop (x:xs) = (x : u, us)
  where (u, us) = chop xs

words :: String -> [String]
words "" = []
words xs = u : words us
  where (u, us) = chop xs
