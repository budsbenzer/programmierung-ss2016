module Tut01 where

-- johannes.osterholzer@tu-dresden.de

-- ghci
-- Ghc, Glasgow Haskell Compiler

mult :: Int -> Int -> Int
mult 0 m = 0
mult n m = m + mult (n-1) m

fac :: Int -> Int
fac 0 = 1
-- fac 1 = 1
fac n = n * fac (n-1)

sumFacs :: Int -> Int -> Int
sumFacs n m
  -- | n == m = fac n
  | n <= m  = fac n + sumFacs (n+1) m
  | otherwise = 0

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib i = fib (i-2) + fib (i-1)


fib'' :: Int -> Int

fib'' i = let fib' :: Int -> Int -> Int -> Int
              fib' 0 n m = n
              -- fib' 1 n m = 1
              fib' i n m = fib' (i-1) m (n+m)
          in fib' i 1 1



k n = k' n 0
  where
    k' n i
      | n == 1    = i
      | even n    = k' (n `div` 2) (i+1)
      | otherwise = k' (3 * n + 1) (i+1)
