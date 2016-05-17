import Parser

int :: Parser Int
int = (token (char '-') >>= \s -> many1 digit >>= \xs -> space >>= \_ -> return (read (s:xs))) +++ natural

-- I forgot that in making above function '-' is constructor for Int
int2 :: Parser Int
int2 = (token (char '-') >>= \_ -> natural >>= \n -> return (-n)) +++ natural

-- the solution of http://www.cs.nott.ac.uk/~pszgmh/solutions.pdf
int' :: Parser Int
int' = (char '-' >>= \_ -> nat >>= \n -> return (-n)) +++ nat
