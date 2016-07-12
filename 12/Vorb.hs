
unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x,y):zs) = (x:xs, y:ys)
  where (xs,ys) = unzip zs


data Tree = Null | Node Int Tree Tree deriving (Eq, Show)

-- 1. Version

subtrees :: Tree -> [Tree]
subtrees Null = [Null]
subtrees t@(Node x l r) = t : (subtrees l ++ subtrees r)

prefixes :: Tree -> [Tree]
prefixes Null = [Null]
prefixes t@(Node x l r) = Null : [Node x u v | u <- prefixes l, v <- prefixes r]

isPatternOf p t = p `elem` concatMap prefixes (subtrees t)

-- 2. Version  

isPrefixOf Null _ = True
isPrefixOf (Node x l r) (Node y u v)
  | x == y = l `isPrefixOf` u && r `isPrefixOf` v
isPrefixOf _ _ = False


isPatternOf' Null Null = True
isPatternOf' _ Null = False
isPatternOf' p t@(Node _ l r) = p `isPrefixOf` t
                                || p `isPatternOf'` l
                                || p `isPatternOf'` r
