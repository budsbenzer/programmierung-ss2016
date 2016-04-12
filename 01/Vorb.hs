module Vorb where

import Prelude hiding (max)


-- Aufgabe 1

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

sumFacs :: Int -> Int -> Int
sumFacs n m
  | n > m      = 0
  | otherwise  = fac n + sumFacs (n+1) m

-- Aufgabe 2

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- oder effizienter:

fib' :: Int -> Int
fib' n = go 1 1 n
  where
    go a _ 0 = a
    go a b n = go b (a+b) (n-1)

-- Aufgabe 3    

cnext :: Int -> Int
cnext n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1

ck :: Int -> Int
ck 1 = 1
ck n = 1 + ck (cnext n)


  
cmax :: Int -> Int  
cmax 0 = 0 -- oder sowas
cmax n = max (cmax (n-1)) (ck n) -- oder cmax (n-1) `max` ck n
  where max a b
          | a > b     = a
          | otherwise = b


collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n : collatz (cnext n)


-- Aufgabe 4
  
countBinTrees :: Int -> Int
countBinTrees 0 = 0
countBinTrees 1 = 1
countBinTrees n = go (n-1)
  where
    go 0 = 0
    go m = go (m-1) + countBinTrees (n-1-m) * countBinTrees m


-- Diese Lösung beruht auf den Catalan-Zahlen (s. z.B. Wikipedia).
countBinTrees' n
  | even n = 0
  | otherwise = let m = (n-1) `div` 2
                in  fac (2*m) `div` (fac m * fac (m+1)) 

