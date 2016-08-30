reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (x:xs) = reverse_ xs ++ [x]

{-

 reverse' :: [a] -> [a] -> [a]
 reverse' xs ys = reverse_ xs ++ ys

 reverse' [] ys

<=> reverse [] ++ ys

<=> [] ++ ys

<=> ys

-- (a hypothesis of the induction)
-- reverse' xs ys <=> reverse xs ++ ys 

 reverse' (x:xs) ys

<=> reverse (x:xs) ++ ys

<=> (reverse xs ++ [x]) ++ ys

<=> reverse xs ++ ([x] ++ ys)

<=> reverse xs ++ (x:ys)

<=> reverse' xs (x:ys)

-}

reverse' :: [a] -> [a] -> [a]
reverse' []     ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)

reverse_p176 :: [a] -> [a]
reverse_p176 xs = reverse' xs []

foldl_reverse :: [a] -> [a]
foldl_reverse = foldl (\xs x -> x : xs) []

{- review the foldl and the foldr

 foldl (⊕) v [x0, x1, ..., xn] = (...((v ⊕ x0) ⊕ x1) ...) ⊕ xn

 foldl :: (a -> b -> a) -> a -> [b] -> a
 foldl _ v []     = v
 foldl f v (x:xs) = foldl f (f v x) xs


 foldr (⊕) v [x0, x1, ..., xn] = x0 ⊕ (x1 ⊕ (...(xn ⊕ v) ...))

 foldr :: (a -> b -> b) -> b -> [a] -> b
 foldr _ v []     = v
 foldr f v (x:xs) = f x (foldr f v xs)

-- reduction of the reverse of p.176

reverse [1,2,3,4,5]

=> foldl (\xs x -> x : xs) [] [1,2,3,4,5]

=> foldl (\xs x -> x : xs) ((\xs x -> x : xs) [] 1) [2,3,4,5]

=> foldl (\xs x -> x : xs) (((\xs x -> x : xs) ((\xs x -> x : xs) [] 1)) 2) [3,4,5]

=> foldl (\xs x -> x : xs) ((\xs x -> x : xs) (((\xs x -> x : xs) ((\xs x -> x : xs) [] 1)) 2) 3) [4,5]

=> foldl (\xs x -> x : xs) ((\xs x -> x : xs) ((\xs x -> x : xs) (((\xs x -> x : xs) ((\xs x -> x : xs) [] 1)) 2) 3) 4) [5]

=> foldl (\xs x -> x : xs) ((\xs x -> x : xs) ((\xs x -> x : xs) ((\xs x -> x : xs) (((\xs x -> x : xs) ((\xs x -> x : xs) [] 1)) 2) 3) 4) 5) []

=> ((\xs x -> x : xs) ((\xs x -> x : xs) ((\xs x -> x : xs) (((\xs x -> x : xs) ((\xs x -> x : xs) [] 1)) 2) 3) 4) 5)

=> 5 : ((\xs x -> x : xs) ((\xs x -> x : xs) (((\xs x -> x : xs) ((\xs x -> x : xs) [] 1)) 2) 3) 4)

=> 5 : (4 : ((\xs x -> x : xs) (((\xs x -> x : xs) ((\xs x -> x : xs) [] 1)) 2) 3))

=> 5 : (4 : (3 : (\xs x -> x : xs) ((\xs x -> x : xs) [] 1) 2))

=> 5 : (4 : (3 : (2 : ((\xs x -> x : xs) [] 1))))

=> 5 : (4 : (3 : (2 : (1 : []))))

=> [5,4,3,2,1]

-}

data Tree = Leaf Int | Node Tree Tree deriving Show

flatten :: Tree -> [Int]
flatten (Leaf n)   = [n]
flatten (Node l r) = flatten l ++ flatten r

{-

 flatten' t ns = flatten t ++ ns

 flatten' (Leaf n) ns

<=> flatten (Leaf n) ++ ns

<=> [n] ++ ns

<=> (n:ns)

-- hepothesis of the induction
-- flatten' t ns = flatten t ++ [ns]


 flatten' (Node l r) ns

<=> flatten (Node l r) ++ ns

<=> (flatten l ++ flatten r) ++ ns

<=> flatten l ++ (flatten r ++ ns)

<=> flatten' l (flatten r ++ ns)

<=> flatten' l (flatten' r ns)

-}

flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n)   ns = n:ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

flat :: Tree -> [Int]
flat t = flatten' t []

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code  = [Op]
data Op    = PUSH Int | ADD deriving Show

exec :: Code -> Stack -> Stack
exec []           s       = s
exec (PUSH n : c) s       = exec c (n:s)
exec (ADD : c)    (m:n:s) = exec c (n+m:s)

comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

-- e = Add (Add (Val 2) (Val 3)) (Val 4)p
