module Fund11 where

data Op = Add | Sub | Mul | Div deriving (Show)

{-
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y
  | y == 0    = False
  | otherwise = x `mod` y == 0
-}

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y
  | y == 0    = False
  | otherwise = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr deriving (Show)

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

{-

subs [1,2,3]
 = (subs [2,3]) ++ map (1:) (subs [2,3])
 = (subs [3] ++ map (2:) subs [3]) ++ map (1:) (subs [2,3])
 = ((subs [] ++ map (3:) subs []) ++ map (2:) subs [3]) ++ map (1:) (subs [2,3])
 = (([[]] ++ map (3:) [[]]) ++ map (2:) subs [3]) ++ map (1:) (subs [2,3])
 = (([[]] ++ [[3]]) ++ map (2:) subs [3]) ++ map (1:) (subs [2,3])
 = (([[],[3]]) ++ map (2:) subs [3]) ++ map (1:) (subs [2,3])
 = (([[],[3]]) ++ map (2:) ([[],[3]])) ++ map (1:) (subs [2,3])
 = (([[],[3]]) ++ ([[2],[2,3]])) ++ map (1:) (subs [2,3])
 = ([[],[3],[2],[2,3]]) ++ map (1:) (subs [2,3])
 = ([[],[3],[2],[2,3]]) ++ map (1:) ([[],[3],[2],[2,3]])
 = ([[],[3],[2],[2,3]]) ++ ([[1],[1,3],[1,2],[1,2,3]])
 = [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-}

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- interleave 3 [4..6] = [[3,4,5,6],[4,3,5,6],[4,5,3,6],[4,5,6,3]]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:ls,rs) | (ls,rs) <- split xs]

{-

split [1,2,3,4]
 = ([1],[2,3,4]):[(1:ls,rs) | (ls,rs) <- split [2,3,4]]
 = ([1],[2,3,4]):
   [(1:ls,rs) | (ls,rs) <- ([2],[3,4]):[(2:ls',rs') | (ls',rs') <- split [3,4]]]
 = ([1],[2,3,4]):
   [(1:ls,rs) | (ls,rs) <- ([2],[3,4]):[(2:ls',rs') | (ls',rs') <- ([3],[4]):[(3:ls'',rs'') | (ls'',rs'') <- split [4]]]]
 = ([1],[2,3,4]):
   [(1:ls,rs) | (ls,rs) <- ([2],[3,4]):[(2:ls',rs') | (ls',rs') <- ([3],[4]):[(3:ls'',rs'') | (ls'',rs'') <- []]]]
 = ([1],[2,3,4]):
   [(1:ls,rs) | (ls,rs) <- ([2],[3,4]):[(2:ls',rs') | (ls',rs') <- ([3],[4]):[]]]
 = ([1],[2,3,4]):
   [(1:ls,rs) | (ls,rs) <- ([2],[3,4]):[(2:ls',rs') | (ls',rs') <- [([3],[4])]]]
 = ([1],[2,3,4]):
   [(1:ls,rs) | (ls,rs) <- ([2],[3,4]):[([2,3],[4])]]
 = ([1],[2,3,4]):
   [(1:ls,rs) | (ls,rs) <- [([2],[3,4]),([2,3],[4])]]
 = ([1],[2,3,4]):
   [([1,2],[3,4]), ([1,2,3],[4])]
 = [([1],[2,3,4]),([1,2],[3,4]), ([1,2,3],[4])]

-}

ops :: [Op]
ops = [Add, Sub, Mul, Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                     lx <- results ls,
                     ry <- results rs,
                     res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply  o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                       (e,m) <- results ns',
                       m == n]
