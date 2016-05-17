module Parser where

import Data.Char
import Control.Applicative hiding (many)
import Control.Monad

infixr 5 +++

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  fmap = undefined

{--
class (Functor f) => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
--}

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

-- parse identifier "   abc   def"
-- parse identifier "   abc   def   "

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

-- p.98
p' :: Parser [Int]
p' = symbol "[" >>= \_ -> natural >>= \n -> many (symbol "," >>= \_ -> natural) >>= \ns -> symbol "]" >>= \_ -> return (n:ns)

-- parse p' " [1, 2, 3] "
-- parse p' " [1, 2, 3,] "

-- expr   ::= term ('+' expr | ε)
-- term   ::= factor ('*' term | ε)
-- factor ::= '(' expr ')' | nat
-- nat    ::= '0' | '1' | '2' | ...

expr :: Parser Int
expr = term >>= \t -> (symbol "+" >>= \_ -> expr >>= \e -> return (t+e)) +++ return t

term :: Parser Int
term = factor >>= \f -> (symbol "*" >>= \_ -> term >>= \t -> return (f*t)) +++ return f

factor :: Parser Int
factor = (symbol "(" >>= \_ -> expr >>= \e -> symbol ")" >>= \_ -> return e) +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
  [(n,[])]  -> n
  [(_,out)] -> error ("unused input " ++ out)
  []        -> error "invalid input"

-- eval "2*3+4"
-- eval "2*(3+4)"
-- eval "  2 * ( 3 + 4 )    "
-- eval "2*3-4"
-- eval "-1"
