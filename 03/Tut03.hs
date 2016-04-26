module Tut03 where

pack :: [Int] -> [Int]
pack [] = []
pack [x] = [x]
pack (x:y:xs)
  | x == y = pack (y:xs)
  | otherwise = x : pack (y:xs)

data Tree a = Branch a (Tree a) (Tree a) | Leaf a
  deriving Show 

depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch _ l r) = min (depth l) (depth r) + 1

paths :: Tree a -> Tree [a]
-- paths (Leaf x) = Leaf [x]
-- paths (Branch x l r) = Branch [x]
paths t = p t []
  where p :: Tree a -> [a] -> Tree [a]
        p (Leaf x) ys = Leaf (ys ++ [x])
        p (Branch x l r) ys =
          Branch (ys ++ [x]) (p l  (ys ++ [x]))
                             (p r (ys ++ [x]))


-- Alternative Lösung:  
tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf x) = Leaf (f x)
tmap f (Branch x l r)
  = Branch (f x) (tmap f l) (tmap f r)

paths' (Leaf x) = Leaf [x]
paths' (Branch x l r)
  = Branch [x] (tmap (\xs -> x:xs) (paths' l)) -- Wir fügen x als erstes Element in jedem Knoten hinzu
               (tmap (\xs -> x:xs) (paths' r))


filter' p = foldr (\x xs -> if p x then x:xs else xs) []

map' f = foldr (\x xs -> f x:xs) []
