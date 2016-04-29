module Vorlesung where

import Prelude hiding ((++),map)

sumList :: [Int] -> Int
sumList []       = 0
sumList (x : xs) = x + sumList xs

double :: [Int] -> [Int]
double []       = []
double (x : xs) = (2 * x) : double xs

rev :: [a] -> [a]
rev []       = []
rev (x : xs) = rev xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
  
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x : xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : ins x ys


(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x : (xs ++ ys)


map :: (a -> a) -> [a] -> [a]
map f [] = []
map f (x : xs) = f x : map f xs

data Tree a = Nil | Node a (Tree a) (Tree a)

mapTree :: (a -> a) -> Tree a -> Tree a
mapTree f Nil = Nil
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node x t1 t2) = collapse t1 ++ [x] ++ collapse t2
