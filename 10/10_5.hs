data Prop =
    Const Bool       -- 恒真命題（T），恒偽命題（⊥）
  | Var   Char       -- 論理変数（ p, q, r, ...）
  | Not   Prop       -- ¬
  | And   Prop Prop  -- ∧
  | Or    Prop Prop  -- ∨
  | Imply Prop Prop  -- ⇒
  | Equiv Prop Prop  -- ⇔
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

type Assoc k v = [(k,v)]
find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k' ]

type Subst = Assoc Char Bool -- Assign（付値）

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = (eval s p <= eval s q) && (eval s q <= eval s p)

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

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

p5 :: Prop
p5 = Imply (Imply (Var 'A') (Var 'B')) (Imply (Imply (Var 'B') (Var 'C')) (Imply (Var 'A') (Var 'C'))) -- (A ⊃ B) ⊃ (B ⊃ C) ⊃ (A ⊃ C)

p6 :: Prop
p6 = Or (Var 'A') (Var 'B')
