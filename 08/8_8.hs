import Parser hiding (expr, term, factor, eval, int)

-- expr   ::= term '+' expr | expr '-' term | term
-- term   ::= base ('*' term | e) | expr '/' base
-- base   ::= factor ('^' term | ε)
-- factor ::= '(' expr ')' | nat
-- nat    ::= '0' | '1' | '2' | ...

{- 上記の素朴な定義を実装すると左再帰の無限ループに陥って一巻の終わり．
     expr => expr '-' term => (expr '-' term) '-' term => ((expr '-' term) '-' term) '-' term => ...
   よって，

     foldl :: (a -> b -> a) -> a -> [b] -> a
     foldl f v []     = v
     foldl f v (x:xs) = foldl f (f v x) xs

  と

     many :: Parser a -> Parser [a]
     many p = many1 p +++ return []

     _>>=_ :: Parser a -> (a -> Parser b) -> Parser b
     p >>= f  = P (\inp -> case parse p inp of
                    []        -> []
                    [(v,out)] -> parse (f v) out)

     many1 :: Parser a -> Parser [a]
     many1 p = p >>= \v -> many p >>= \vs -> return (v:vs)

  を用いて expr を実装し直す．-}

int :: Parser Int
int = (token (char '-') >>= \_ -> natural >>= \n -> return (-n)) +++ natural

-- https://gist.github.com/yamamotoj/f8ddb5cead527344268a を natural の部分以外全部（本質部分）を真似しました．
expr :: Parser Int
--expr = term >>= \t -> (symbol "+" >>= \_ -> expr >>= \e -> return (t+e)) +++ return t
expr = term >>= \t -> (symbol "+" >>= \_ -> expr >>= \e -> return (t+e))
                      +++ (many (symbol "-" >>= \_ -> term >>= \n -> return n) >>= \ss -> return (foldl (-) t ss))
                      +++ return t

term :: Parser Int
term = factor >>= \f -> (symbol "*" >>= \_ -> term >>= \t -> return (f*t)) +++ return f

factor :: Parser Int
factor = (symbol "(" >>= \_ -> expr >>= \e -> symbol ")" >>= \_ -> return e) +++ int

eval :: String -> Int
eval xs = case parse expr xs of
  [(n,[])]  -> n
  [(_,out)] -> error ("unused input " ++ out)
  []        -> error "invalid input"
