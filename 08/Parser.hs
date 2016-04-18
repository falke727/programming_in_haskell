module Parser where

import Data.Char
import Control.Monad

infixr 5 +++

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f  = P (\inp -> case parse p inp of
                 []        -> []
                 [(v,out)] -> parse (f v) out)

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
