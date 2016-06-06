import Parser hiding (expr, term, factor, eval)

-- expr   ::= term '+' expr | expr '-' term | term
-- term   ::= base ('*' term | e) | expr '/' base
-- base   ::= factor ('^' term | ε)
-- factor ::= '(' expr ')' | nat
-- nat    ::= '0' | '1' | '2' | ...

ufloat :: Parser Float
ufloat = many1 digit >>= \xs -> ((symbol ".") >>= \_ -> many1 digit >>= \ys -> return $ read (xs ++ "." ++ ys)) +++ (return $ read xs)

float :: Parser Float
float = ((char '-') >>= \_ -> ufloat >>= \f -> return (-f)) +++ ufloat

floating :: Parser Float
floating = token float

expr :: Parser Float
expr = (term >>= \t -> (symbol "+" >>= \_ -> expr >>= \e -> return (t+e))) +++ (expr >>= \e -> symbol "-" >>= \_ -> term >>= \t -> return (e-t)) +++ term

term :: Parser Float
term = base >>= \b -> (symbol "*" >>= \_ -> term >>= \t -> return (b*t)) +++ (symbol "/" >>= \_ -> term >>= \t -> return (b/t)) +++ return b

base :: Parser Float
base = factor >>= \f -> (symbol "^" >>= \_ -> term >>= \t -> return (f**t)) +++ return f

factor :: Parser Float
factor = (symbol "(" >>= \_ -> expr >>= \e -> symbol ")" >>= \_ -> return e) +++ floating

eval :: String -> Float
eval xs = case parse expr xs of
  [(f,[])] -> f
  [(_,out)] -> error ("unused input " ++ out)
  [] -> error "invalid input"
