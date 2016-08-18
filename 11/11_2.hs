isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []     _  = True
isChoice _      [] = True
isChoice (x:xs) ys = if elem x ys then isChoice xs (del x ys) else False
  where
    del :: Eq a => a -> [a] -> [a]
    del x []     = []
    del x (y:ys) = if x == y then ys else del x ys


choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):map(y:)(interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs
