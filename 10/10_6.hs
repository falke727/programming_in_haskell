import Data.Char
import Control.Applicative hiding (many, Const)
import Control.Monad

infixr 5 +++

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  fmap = undefined

instance Applicative Parser where
  pure    = undefined
  p <*> x = undefined

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f  = P (\inp -> case parse p inp of
                 []        -> []
                 [(v,out)] -> parse (f v) out)

{--
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
--}

instance Alternative Parser where
  empty = undefined
  (<|>) = undefined

instance MonadPlus Parser where
  mzero       = P (\inp -> [])
  p `mplus` q = P (\inp -> case parse p inp of
                    []        -> parse q inp
                    [(v,out)] -> [(v,out)])

failure :: Parser a
failure = mzero

-- parse failure = []

item :: Parser Char
item = P (\inp -> case inp of
           []     -> []
           (x:xs) -> [(x,xs)])

-- parse item "abc" = [('a',"bc")]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q


p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return (x, y)

-- parse p "abc" = [(('a','c'),"")]
-- parse p "bc" = []

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

-- parse digit "iuh"
-- parse digit "123"

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string [] = return []
string (x:xs) = char x >>= \_ -> string xs >>= \_ -> return (x:xs)

-- parse (string "\nab") "\nabccde"

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v -> many p >>= \vs -> return (v:vs)

-- parse (many digit) "123abc"
-- parse (many (string "123")) "1231234abc"

ident :: Parser String
ident = lower >>= \x -> many alphanum >>= \xs -> return (x:xs)

nat :: Parser Int
nat = many1 digit >>= \xs -> return (read xs)

space :: Parser ()
space = many (sat isSpace) >>= \_ -> return ()

token :: Parser a -> Parser a
token p = space >>= \_ -> p >>= \v -> space >>= \_ -> return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

---- ----

-- 論理結合子の結合順位は下記の定義順，¬, ∧, ∨, ⇒, ⇔
data Prop =
    Const Bool       -- 恒真命題（T），恒偽命題（⊥）
  | Var   Char       -- 論理変数（ p, q, r, ...）
  | Not   Prop       -- ¬
  | And   Prop Prop  -- ∧　右結合とする（多分どちらでも問題ない）
  | Or    Prop Prop  -- ∨　右結合とする（多分どちらでも問題ない）
  | Imply Prop Prop  -- ⇒　右結合
  | Equiv Prop Prop  -- ⇔　右結合とする（多分どちらでも問題ない）
  deriving (Show)

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

-- prop0 ::= prop1 ("<=>" prop0 | ε)
-- prop1 ::= prop2 ("=>"  prop1 | ε)
-- prop2 ::= prop3 ("||"  prop2 | ε)
-- prop3 ::= prop4 ("&&"  prop3 | ε)
-- prop4 ::= '(' prop0 ')' | '!' prop0 | atom
-- atom  ::= "var A"       | "top"     | "bot" -- where 'A' is the meta variable

prop0 :: Parser Prop
prop0 = prop1 >>= \p1 -> (symbol "<=>" >>= \_ -> prop0 >>= \p0 -> return (Equiv p1 p0)) +++ return p1

prop1 :: Parser Prop
prop1 = prop2 >>= \p2 -> (symbol "=>" >>= \_ -> prop1 >>= \p1 -> return (Imply p2 p1)) +++ return p2

prop2 :: Parser Prop
prop2 = prop3 >>= \p3 -> (symbol "||" >>= \_ -> prop2 >>= \p2 -> return (Or p3 p2)) +++ return p3

prop3 :: Parser Prop
prop3 = prop4 >>= \p4 -> (symbol "&&" >>= \_ -> prop3 >>= \p3 -> return (And p4 p3)) +++ return p4

prop4 :: Parser Prop
prop4 = (symbol "(" >>= \_ -> prop0 >>= \p0 -> symbol ")" >>= \_ -> return p0) +++ (symbol "!" >>= \_ -> prop0 >>= \p0 -> return (Not p0)) +++ atom

atom :: Parser Prop
atom = (symbol "var" >>= \_ -> alphanum >>= \a -> return (Var a)) +++ (symbol "top" >>= \_ -> return (Const True)) +++ (symbol "bot" >>= \_ -> return (Const False))

evalIsTaut :: String -> Bool
evalIsTaut xs = case parse prop0 xs of
  [(p,[])]  -> isTautology p
  [(_,out)] -> error ("unused input " ++ out)
  []        -> error "invalid input"
