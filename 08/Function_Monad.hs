import Control.Monad

type Parser a = ((->) String) [(a, String)]

failure :: Parser a   -- Parser は String 型の値をもらう関数なので failure も String 型の値にのみ適用可
failure = \inp -> []

item :: ((->) String) [(Char, String)]
--item :: Parser Char
item = \inp -> case inp of
  []     -> []
  (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

{--
const :: a -> b -> a
const x _ = x

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

instance Monad ((->) r) where
    return = const
    f >>= k = \ r -> k (f r) r

上記の \r -> k (f r) r における結合順序は \r -> (k (f r)) r である．

--}

-- p :: ((->) String) [((Char, Char), String)]
--p :: Parser (Char, Char)
p = item >>= \x -> item >>= \_ -> item >>= \y -> \z -> [[(fst (head x))],(snd (head x))]
