import Prelude hiding (Right, Left)

type String' = [Char]
type Board = [Pos]
type Pos = (Int,Int)

-- error -- type Tree = (Int,[Tree])

type Parser a = String -> [(a,String)]
data World = World -- The Data type "World" does not exist.
type IO a = World -> (a, World)

type Assoc k v = [(k,v)]

hoge :: Assoc Int String
hoge = [(1,"kuwahara"), (2,"elian"), (3,"miyazaki"), (4,"tsutsugoh")]

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k' ]

data Bool' = True' | False'

data Move = Left | Right | Up | Down

move :: Move -> Pos -> Pos
move Left  (x,y) = (x-1,y)
move Right (x,y) = (x+1,y)
move Up    (x,y) = (x,y+1)
move Down  (x,y) = (x,y-1)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

moveExample = [Right,Up,Right,Up,Right,Down,Left]
-- moves moveExample (0,0)

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

data Maybe' a = Just' a | Nothing'

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead []     = Nothing
safehead (x:xs) = Just x

data Nat = Zero | Suc Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero    = 0
nat2int (Suc x) = 1 + (nat2int x)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Suc (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

add' :: Nat -> Nat -> Nat
add' Zero    n = n
add' (Suc m) n = Suc (add' m n)

data List a = Nil | Cons a (List a) deriving (Show)

len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

tarao :: List Int
tarao = Cons 4 (Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil))))

data Tree = Leaf Int | Node Tree Int Tree deriving (Show)

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r) = (occurs m l) || m == n || (occurs m r)

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

-- The Tree defined in this program is sorted.
occurs' :: Int -> Tree -> Bool
occurs' m (Leaf n) = m == n
occurs' m (Node l n r)
  | m == n    = True
  | m <  n    = occurs' m l
  | otherwise = occurs' m r

data TreeA a = LeafA a | NodeA (TreeA a) (TreeA a)
data TreeB a = LeafB | NodeB (TreeB a) a (TreeB a)
data TreeC a b = LeafC a | NodeC (TreeC a b) b (TreeC a b)
data TreeD a = NodeD a [TreeD a]

tD :: TreeD Int
tD = NodeD (-2) [NodeD (-1) [NodeD 0 [NodeD 1 [], NodeD 2 [], NodeD 3 []]]]

data Prop =
    Const Bool
  | Var   Char
  | Not   Prop
  | And   Prop Prop
  | Imply Prop Prop
  deriving (Show)

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A')) -- A ∧ ¬A

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A') -- (A ∧ B) ⊃ A

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B')) -- A ⊃ (A ∧ B)

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B') -- (A ∧ (A ⊃ B)) ⊃ B

top :: Prop
top = Const True

bot :: Prop
bot = Const False

type Subst = Assoc Char Bool -- Assign（付値）

katsuo :: Subst
katsuo = [('A', False), ('B', True)]

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bools' :: Int -> [[Bool]]
bools' n = map (map conv . make n . int2bin) [0..limit]
  where
    limit = (2^n) - 1
    make n bs = take n (bs ++ repeat 0)
    conv 0 = False
    conv 1 = True

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

isTautology :: Prop -> Bool
isTautology p = and [eval s p | s <- substs p]

isSatisfiable :: Prop -> Bool
isSatisfiable p = or [eval s p | s <- substs p]

data Expr = Val Int | Add Expr Expr deriving Show

value :: Expr -> Int
value (Val x)   = x
value (Add x y) = (value x) + (value y)

type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval' :: Expr -> Cont -> Int
eval' (Val n)   c = exec  c n
eval' (Add x y) c = eval' x ((EVAL y) : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec ((EVAL y) : c) n = eval' y ((ADD n) : c)
exec ((ADD n)  : c) m = exec  c (n + m)

value' :: Expr -> Int
value' e = eval' e []

{-

   value (Add (Add (Val 2) (Val 3)) (Val 4))

=> eval' (Add (Add (Val 2) (Val 3)) (Val 4)) []

=> eval' (Add (Val 2) (Val 3)) [(Eval (Val 4))]

=> eval' (Val 2) [(Eval (Val 3)), (Eval (Val 4))]

=> exce  [(Eval (Val 3)), (Eval (Val 4))] 2

=> eval' (Val 3) [(ADD 2), (Eval (Val 4))]

=> exec  [(ADD 2), (Eval (Val 4))] 3

=> exec  [(Eval (Val 4))] 5

=> eval' (Val 4) [ADD 5]

=> exec  [ADD 5] 4

=> exec  [] 9

=> 9

-}
