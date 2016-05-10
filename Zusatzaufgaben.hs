module Zusatzaufgaben where

import Prelude hiding (any, null, foldl)

--------------------------------------------------------------------------------
-- Übung 02, Zusatzaufgabe 1
--------------------------------------------------------------------------------

pack [] = []
pack (x:xs) = ys : pack zs
  where
    (ys, zs) = takeall x (x:xs)
    takeall _ [] = ([], [])
    takeall x (y:ys)
      | x == y = let (us, vs) = takeall x ys
                 in  (y:us, vs)
      | otherwise = ([], (y:ys))

encode xs = e' (pack xs)
  where e' [] = []
        e' (y:ys) = (head y, length y) : e' ys

decode [] = []
decode ((n, x) : xs) = times n x : decode xs
  where
    times 0 x = []
    times n x = x : times (n-1) x

rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate xss@(x:xs) n
  | n == 0 = xss
  | n < 0  = rotate xss (length xss + n)
  | otherwise = rotate (xs ++ [x]) (n - 1)

--------------------------------------------------------------------------------
-- Übung 2, Zusatzaufgabe 2
--------------------------------------------------------------------------------

foldl f a [] = a
foldl f a (x:xs) = foldl f (f a x) xs

-- Ausprobieren: foldl' (\x y -> "(" ++ x ++ y ++")") "e" $ map show [1..10]

--------------------------------------------------------------------------------
-- Übung 3, Zusatzaufgabe 2
--------------------------------------------------------------------------------

isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf xs@(x : xs') (y : ys)
  | x == y    = isSubseqOf xs' ys
  | otherwise = isSubseqOf xs  ys
isSubseqOf []      _  = True
isSubseqOf (_ : _) [] = False


-- let g = (\ x y -> 2 * x + y)

--   h [1, 2] [3, 4, 5]
-- = zipWith g [1, 2] [3, 4, 5]
-- = g 1 3     : zipWith g [2] [4, 5]
-- = 2 * 1 + 3 : g 2 4     : zipWith g [] [5]
-- = 5         : 2 * 2 + 4 : []
-- = 5         : 8         : []
-- = [5, 8]

data Tree a = Node a [Tree a]

isPathOf :: Eq a => [a] -> Tree a -> Bool
isPathOf (x : xs) (Node y ts)
  | x == y    = null xs || any (isPathOf xs) ts
  | otherwise = False
isPathOf [] _ = False

any :: (a -> Bool) -> [a] -> Bool
any p (x : xs) = p x || any p xs
any _ []       = False

null :: [a] -> Bool
null []      = True
null (_ : _) = False

--------------------------------------------------------------------------------
-- Übung 3, Zusatzaufgabe 3
--------------------------------------------------------------------------------

data Expr = Lit Int
          | Var Char
          | Add Expr Expr
          | Mul Expr Expr
          | Exp Expr Int

type Assignment = Char -> Int

eval :: Expr -> Assignment -> Int
eval (Lit n) _     = n
eval (Var c) a     = a c
eval (Add e1 e2) a = eval e1 a + eval e2 a
eval (Mul e1 e2) a = eval e1 a * eval e2 a
eval (Exp e n) a   = eval e a ^ n

display :: Expr -> String
display (Lit n)     = show n
display (Var c)     = [c]
display (Add e1 e2) = "(" ++ display e1 ++ " + " ++ display e2 ++ ")"
display (Mul e1 e2) = display e1 ++ " * " ++ display e2
display (Exp e n)   = "(" ++ display e ++ ")" ++ "^" ++ show n

diff :: Expr -> Char -> Expr
diff (Lit _) _ = Lit 0          -- dc/dx = 0
diff (Var x) y
  | x == y     = Lit 1          -- dx/dx = 1
  | otherwise  = Lit 0          -- dy/dx = 0
-- d(f + g)/dx = df/dx + dg/dx
diff (Add e1 e2) x = Add (diff e1 x) (diff e2 x)
-- d(f * g)/dx = df/dx * g + f * dg/dx
diff (Mul e1 e2) x = Add (Mul (diff e1 x) e2)
                         (Mul e1 (diff e2 x))
-- d f^n/dx = n*f^(n-1) * df/dx (Nachdifferenzieren!)
diff (Exp e n) x   = (Mul (Mul (Lit n) (Exp e (n-1))) (diff e x))
