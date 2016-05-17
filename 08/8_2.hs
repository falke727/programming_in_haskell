import Parser

comment :: Parser ()
comment = token (string "--") >>= \_ -> token (many (sat (/='\n'))) >>= \_ -> return ()

-- the solution of http://www.cs.nott.ac.uk/~pszgmh/solutions.pdf
comment' :: Parser ()
comment' =
  do string "--"
     many (sat (/= '\n'))
     return ()

