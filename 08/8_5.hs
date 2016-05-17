import Parser

-- implement the Parser defined in p.100 

expr2 :: Parser Int
expr2 = (term2 >>= \t -> (symbol "+" >>= \_ -> expr2 >>= \e -> return (t+e))) +++ term2

term2 :: Parser Int
term2 = (factor2 >>= \f -> (symbol "*" >>= \_ -> term2 >>= \t -> return (f*t))) +++ factor2

factor2 :: Parser Int
factor2 = (symbol "(" >>= \_ -> expr2 >>= \e -> symbol ")" >>= \_ -> return e) +++ natural

eval2 :: String -> Int
eval2 xs = case parse expr2 xs of
  [(n,[])]  -> n
  [(_,out)] -> error ("unused input " ++ out)
  []        -> error "invalid input"
