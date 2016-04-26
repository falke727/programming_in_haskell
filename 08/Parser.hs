import Data.Char
import Control.Applicative
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

p' :: Parser (Char, Char)
p' = item >>= (\x -> item >>= (\_ -> item >>= \y -> return (x, y)))

-- >>= : ((->) r) a -> (a -> (((->) r) b)) -> ((->) r) b
--
-- h >>= f = \w -> f (h w) w
--
--
--    r    (a -> (((->) r) b))  |  (((->) r) a)   r   |  r
--                              |                     |  
--  λ. w           f           |       h         w   |  w
--                                -------------------
--                                          a
--
--                                         h w
--          -----------------------------------
--                       ((->) r) b
--
--                        f (h w) 
--                      ------------------------------------
--                                  b
--
--                               f (h w) w
-- ----------------------------------------
--             r -> b
--           λ. w -> f (h w) w

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else failure

sat' :: (Char -> Bool) -> Parser Char
sat' p = item >>= \x -> if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

-- parse digit "iuh"
-- parse digit "123"
