choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

{-

choices [1,2,3]
  = concat (map perms (subs [1,2,3])
  = concat (map perms [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]])
  = concat [[[]],[[3]],[[2]],[[2,3],[3,2]],[[1]],[[1,3],[3,1]],[[1,2],[2,1]],[[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]]
  = [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-}

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

{-  review the list comprehension


instance Monad [] where
  return = (:[])
  (>>=)  = concatMap

  concat (map show [1..5])
  concatMap show [1..5] = "12345"
  [1..5] >>= show

    [ f n | n <- ns, P n ]

<=> ns >>= \n -> if P n then [ f n ] else []
<=> concatMap (\n -> if P n then [f n] else []) ns
<=> concat (map (\n -> if P n then [f n] else []) ns)
 
  in the above, P is a some condition for the value of the type of n

-}


-- choices' :: [a] -> [[a]]
choices' xs = concat [ perms x | x <- (subs xs) ]

-- [ perms x | x <- (subs [1,2,3]) ] >>= \y -> y

-- choices xs = (subs xs) >>= perms
-- choices xs = concatMap perms (subs xs)
-- choices xs = concat (map perms (subs xs))
