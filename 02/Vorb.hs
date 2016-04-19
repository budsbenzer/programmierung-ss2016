module Vorb where

import Prelude hiding (words, unwords)

comp :: [Int] -> [Int] -> Bool
comp [] [] = True
comp (x:xs) (y:ys)
  | x == y = comp xs ys
comp _ _ = False    

merge :: [Int] -> [Int] -> [Int]
merge [] xs = xs
merge xs [] = xs
merge xss@(x:xs) yss@(y:ys)
  | x < y = x : merge xs yss
  | otherwise = y : merge xss ys

unwords :: [String] -> String
unwords [] = ""
unwords [s] = s
unwords (w:ws) = w ++ " " ++ unwords ws

words :: String -> [String]
words [] = []
words ws = u : words us
  where (u, us) = breakAtSpace ws
        breakAtSpace [] = ([],[])
        breakAtSpace (' ':cs) = ([], cs)
        breakAtSpace (c:cs) = let (v,vs) = breakAtSpace cs
                              in  (c:v, vs)

-- Problem wenn Wörter von mehr als einem ' ' getrennt werden.
-- Lösung: breakAtSpace (dropSpaces ws)
-- wobei dropSpaces (' ':ws) = dropSpaces ws
--       dropSpaces ws = ws


-- Schrittweise entwickeln, erst filter even, dann map, dann foldr
f :: [Int] -> Int
f xs = foldr (*) 1 (map (\x -> x * x) (filter even xs))

-- Schöner:
f' = foldr (*) 1 . map square . filter even
  where square x = x * x

map' f xs = foldr step [] xs
  where step x xs = f x : xs
        

filter' p xs = foldr step [] xs
  where step x xs = if p x then x:xs else xs

-- Eta-Reduktion (part. Applikation): filter' p = foldr step [] where...

-- Zusatzaufgabe 1        

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

-- Zusatzaufgabe 2
foldl' f a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs

-- Ausprobieren: foldl' (\x y -> "(" ++ x ++ y ++")") "e" $ map show [1..10]
-- Geht natürlich nur polymorph, bzw. ohne Typannotation
