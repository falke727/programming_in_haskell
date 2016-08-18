data Op = Add | Sub | Mul | Div deriving Show


valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- valid Add 3 4

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- apply Div 5 4

data Expr = Val Int | App Op Expr Expr deriving Show -- Val and App are constructors.
  

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- eval (App Add (Val 3) (Val 4))
-- eval (App Sub (Val 3) (Val 4))

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

{-

    subs [1..3]

  = (subs [2,3]) ++ map (1:) (subs [2,3])

  = ((subs [3]) ++ map (2:) (subs [3])) ++ map (1:) (subs [2,3])

  = ((subs [] ++ map (3:) subs []) ++ map (2:) (subs [3])) ++ map (1:) (subs [2,3])

  = ((subs [] ++ map (3:) subs []) ++ map (2:) (subs [3])) ++ map (1:) (subs [2,3])

  = (([[]] ++ map (3:) [[]]) ++ map (2:) (subs [3])) ++ map (1:) (subs [2,3])

  = (([[]] ++ map (3:) [[]]) ++ map (2:) (subs [3])) ++ map (1:) (subs [2,3])

  = (([[]] ++ [[3]]) ++ map (2:) (subs [3])) ++ map (1:) (subs [2,3])

  = [[],[3]] ++ [[2],[2,3]] ++ map (1:) (subs [2,3])

  = [[],[3],[2],[2,3]] ++ [[1],[1,3],[1,2],[1,2,3]]

  = [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-}

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):map(y:)(interleave x ys)

{-

    interleave 1 [2,3,4]

  = (1:2:[3,4]):map(2:)(interleave 1 [3,4])

  = [1,2,3,4]:map(2:)( (1:3:[4]):map(3:)(interleave 1 [4]))

  = [1,2,3,4]:map(2:)( [1,3,4]:map(3:)(interleave 1 [4]))

  = [1,2,3,4]:map(2:)( [1,3,4]:map(3:)( ((1:4:[]):map(4:)(interleave 1 [])) ))

  = [1,2,3,4]:map(2:)( [1,3,4]:map(3:)( ([1,4]):map(4:)(interleave 1 [])) )

  = [1,2,3,4]:map(2:)( [1,3,4]:map(3:)( ([1,4]):map(4:)([[1]])) )

  = [1,2,3,4]:map(2:)( [1,3,4]:map(3:)( [[1,4],[4,1]] ) )

  = [1,2,3,4]:map(2:)( [1,3,4]:[[3,1,4],[3,4,1]] )

  = [1,2,3,4]:map(2:)[[1,3,4],[3,1,4],[3,4,1]]

  = [1,2,3,4]:[[2,1,3,4],[2,3,1,4],[2,3,4,1]]

  = [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]

-}

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

{-

    perms [1,2,3]

  = concat (map (interleave 1) (perms [2,3]))

  = concat (map (interleave 1) (concat (map (interleave 2) (perms [3]))))

  = concat (map (interleave 1) (concat (map (interleave 2) (concat (map (interleave 3) (perms []))))))

  = concat (map (interleave 1) (concat (map (interleave 2) (concat (map (interleave 3) [[]])))))

  = concat (map (interleave 1) (concat (map (interleave 2) (concat [[3]]))))

  = concat (map (interleave 1) (concat (map (interleave 2) [[3]])))

  = concat (map (interleave 1) (concat [(interleave 2) [3]]))

  = concat (map (interleave 1) (concat [(2:3:[]):map(3:)(interleave 2 [])]))

  = concat (map (interleave 1) (concat [(2:3:[]):map(3:)[[2]]]))

  = concat (map (interleave 1) (concat [(2:3:[]):[[3,2]]]))

  = concat (map (interleave 1) (concat [([2,3]):[[3,2]]]))

  = concat (map (interleave 1) (concat [[[2,3],[3,2]]]))

  = concat (map (interleave 1) [[2,3],[3,2]])

  = concat ([(interleave 1) [2,3],(interleave 1) [3,2]])

  = concat ([(interleave 1) [2,3], (1:3:[2]):map(3:)(interleave 1 [2])])

  = concat ([(interleave 1) [2,3], (1:3:[2]):map(3:)((1:2:[]):map(2:)(interleave 1 []))])

  = concat ([(interleave 1) [2,3], (1:3:[2]):map(3:)((1:2:[]):map(2:)[[1]])])

  = concat ([(interleave 1) [2,3], (1:3:[2]):map(3:)((1:2:[]):[[2,1]])])

  = concat ([(interleave 1) [2,3], (1:3:[2]):map(3:)([1,2]:[[2,1]])])

  = concat ([(interleave 1) [2,3], (1:3:[2]):[[3,1,2],[3,2,1]]])

  = concat ([(interleave 1) [2,3], [1,3,2]:[[3,1,2],[3,2,1]]])

  = concat ([(interleave 1) [2,3], [[1,3,2],[3,1,2],[3,2,1]]])

  = concat ([(1:2:[3]):map(2:)(interleave 1 [3]), [[1,3,2],[3,1,2],[3,2,1]]])

  = concat ([(1:2:[3]):map(2:)((1:3:[]):map(3:)(interleave 1 [])), [[1,3,2],[3,1,2],[3,2,1]]])

  = concat ([(1:2:[3]):map(2:)((1:3:[]):map(3:)[[1]]), [[1,3,2],[3,1,2],[3,2,1]]])

  = concat ([(1:2:[3]):map(2:)((1:3:[]):[[3,1]]), [[1,3,2],[3,1,2],[3,2,1]]])

  = concat ([(1:2:[3]):map(2:)([1,3]:[[3,1]]), [[1,3,2],[3,1,2],[3,2,1]]])

  = concat ([(1:2:[3]):map(2:)[[1,3],[3,1]], [[1,3,2],[3,1,2],[3,2,1]]])

  = concat ([(1:2:[3]):[[2,1,3],[2,3,1]], [[1,3,2],[3,1,2],[3,2,1]]])

  = concat ([[1,2,3]:[[2,1,3],[2,3,1]], [[1,3,2],[3,1,2],[3,2,1]]])

  = concat [[[1,2,3],[2,1,3],[2,3,1]], [[1,3,2],[3,1,2],[3,2,1]]]

  = [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-}

choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)) -- (1+50) * (25-10)
-- [1,3,7,10,25,50]
-- 765

split :: [a] -> [([a],[a])]
split []      = []
split [_]     = []
split (x:xs) = ([x],xs):[(x:ls,rs) | (ls,rs) <- split xs]

{-

    split [1,2,3,4]

  = ([1],[2,3,4]):[(1:ls,rs) | (ls,rs) <- split [2,3,4]]

  = ([1],[2,3,4]):[(1:ls,rs) | (ls,rs) <- ([2],[3,4]):[(2:ls',rs') | (ls',rs') <- split [3,4]]]

  = ([1],[2,3,4]):[(1:ls,rs) | (ls,rs) <- ([2],[3,4]):[(2:ls',rs') | (ls',rs') <- ([3],[4]):[(3:ls'',rs'') | (ls'',rs'') <- split [4]]]]

  = ([1],[2,3,4]):[(1:ls,rs) | (ls,rs) <- ([2],[3,4]):[([2,3],[4])]]

  = ([1],[2,3,4]):[(1:ls,rs) | (ls,rs) <- [([2],[3,4]),([2,3],[4])]]

  = ([1],[2,3,4]):[(1:ls,rs) | (ls,rs) <- [([2],[3,4]),([2,3],[4])]]

  = ([1],[2,3,4]):[([1,2],[3,4]),([1,2,3],[4])]

  = [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

-}

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- solutions [1,3,7,10,25,50] 765

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod`y == 0

combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine'' lx ry]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns, (e,m) <- results' ns', m == n]
