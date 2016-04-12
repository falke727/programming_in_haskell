type Parser a = String -> [(a, String)]

--return' :: a -> Parser a
return' :: a -> (String -> [(a, String)])
return' v = \inp -> [(v, inp)]

--failure :: String -> [(a, String)]
failure :: Parser a
failure = \inp -> []

--item :: String -> [(Char, String)]
item :: Parser Char
item = \inp -> case inp of
  []     -> []
  (x:xs) -> [(x,xs)]

{--

return' 3        "abc" 
return' (Just 8) ""
return' Nothing  ['a'..'z']

failure "This is a test"
failure "hehehenohe"

item "hohoho"
item "K"
item []

--}

--

--parse :: Parser a -> Parser a
parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

{--

parse (return' 1)        "abc"
parse failure            "abc"
parse item               []
parse item               "abc"
parse (return' (Just 3)) "hohoho"
parse (return' Nothing)  "abc"

--}

(>>==) :: Parser a -> (a -> Parser b) -> Parser b
p >>== f = \inp -> case parse p inp of
  []         -> []
  [(v, out)] -> parse (f v) out

p :: Parser (Char, Char)
p = item >>== (\x -> item >>== (\_ -> item >>== (\y -> return' (x,y))))

{--

parse p "abcdef"
parse p "ab"

--}

