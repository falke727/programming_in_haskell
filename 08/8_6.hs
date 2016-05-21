import Parser

ufloat :: Parser Float
ufloat = many1 digit >>= \xs -> (symbol ".") >>= \_ -> many1 digit >>= \ys -> return $ read (xs ++ "." ++ ys)

float :: Parser Float
float = ((char '-') >>= \_ -> ufloat >>= \f -> return (-f)) +++ ufloat

-- expr   ::= term ('+' expr | '-' expr | e)
-- term   ::= factor ('*' term | '/' expr | e)
-- factor ::= '(' expr ')' | nat
-- nat    ::= '0' | '1' | '2' | ...

floating :: Parser Float
floating = token float

expr :: Parser Float
expr = term >>= \t -> (symbol "+" >>= \_ -> expr >>= \e -> return (t+e)) +++ return t

term :: Parser Float
term = factor >>= \f -> (symbol "*" >>= \_ -> term >>= \t -> return (f*t)) +++ return f

factor :: Parser Float
factor = (symbol "(" >>= \_ -> expr >>= \e -> symbol ")" >>= \_ -> return e) +++ floating

eval :: String -> Float
eval xs = case parse expr xs of
  [(f,[])] -> f
  [(_,out)] -> error ("unused input " ++ out)
  [] -> error "invalid input"
