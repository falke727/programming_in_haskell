import Data.Char
import Prelude hiding (return, (>>=))

type Parser a = ((->) String) [(a, String)]

--return' :: a -> Parser a
return :: a -> (String -> [(a, String)])
return v = \inp -> [(v, inp)]

--failure :: String -> [(a, String)]
failure :: Parser a
failure = \inp -> []

--item :: String -> [(Char, String)]
item :: Parser Char
item = \inp -> case inp of
  []     -> []
  (x:xs) -> [(x,xs)]

{--
  return 3        "abc" 
  return (Just 8) ""
  return Nothing  ['a'..'z']

  failure "This is a test"
  failure "hehehenohe"

  item "hohoho"
  item "K"
  item []
--}

--parse :: Parser a -> Parser a
parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

{--
  parse (return 1)        "abc"
  parse failure            "abc"
  parse item               []
  parse item               "abc"
  parse (return (Just 3)) "hohoho"
  parse (return Nothing)  "abc"
--}

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
  []         -> []
  [(v, out)] -> parse (f v) out

p :: Parser (Char, Char)
p = item >>= (\x -> item >>= (\_ -> item >>= (\y -> return (x,y))))

{--
  parse p "abcdef"
  p "abcdef"
  item >>= (\x -> item >>= (\_ -> item >>= (\y -> return (x,y)))) "abcdef"
  case parse item "abcdef" of  -- item "abcdef" --> [(a, "bcdef")]
    [] -> []
    [(v,out)] -> parse ((\x -> item >>= (\_ -> item >>= (\y -> return (x,y)))) v) out
  parse ((\x -> item >>= (\_ -> item >>= (\y -> return (x,y)))) 'a') "bcdef"
  ((\x -> item >>= (\_ -> item >>= (\y -> return (x,y)))) 'a') "bcdef"
  item >>= (\_ -> item >>= (\y -> return ('a',y))) "bcdef"
  case parse item "bcdef" of  -- item "bcdef" --> [(b, "cdef")]
    [] -> []
    [(v,out)] -> parse ((\_ -> item >>= (\y -> return (a,y))) v) out
  parse ((\_ -> item >>= (\y -> return (a,y))) 'b') "cdef"
  ((\_ -> item >>= (\y -> return (a,y))) 'b') "cdef"
  item >>= (\y -> return ('a',y)) "cdef"
  case parse item "cdef" of
    [] -> []
    [(v,out)] -> parse ((\y -> return (a,y)) v) out
  parse ((\y -> return ('a','y')) 'c') "def"
  ((\y -> return ('a',y)) 'c') "def"
  return ('a','c') "def"
  [(('a','c'),"def")]
--}

{--
  parse p "abcdef"
  parse p "ab"
--}

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
  []        -> parse q inp
  [(v,out)] -> [(v,out)]

{--
  parse (item +++ return 'd') "abc"
  parse (failure +++ return 'd') "abc"
  parse (failure +++ failure) "abc"
--}

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

{--
  digit "012345"
  sat isDigit "012345"
  (item >>= \x -> if isDigit x then return x else failure) "012345"
  (\inp -> case parse item inp of
     [] -> []
     [(v,out)] -> parse ((\x -> if isDigit x then return x else failure) v) out) "012345"
  parse ((\x -> if isDigit x then return x else failure) '0') "12345"
  ((\x -> if isDigit x then return x else failure) '0') "12345"
  (if isDigit '0' then return '0' else failure) "12345"
  return '0' "12345"
  [('0',"12345")]
--}

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

{--
  parse digit "123"
  parse digit "abc"
  parse lower "abc"
  parse lower "Abc"
  parse upper "Abc"
  parse upper "123"
  parse alphanum "\n\n"
  parse alphanum "3\n\n"
  parse (char 'a') "123"
  parse (char '\0') "\0 345"
--}

string :: String -> Parser String
string []     = return []
string (x:xs) = char x >>= \_ -> string xs >>= \_ -> return (x:xs)

{--
  parse (string "abc") "abcdef"
  (string "abc") "abcdef"
  (string ('a':"bc")) "abcdef"
  (char 'a' >>= \_ -> (string "bc" >>= \_ -> return ('a':"bc"))) "abcdef"
  (\inp -> case parse (char 'a') inp of
      [] -> []
      [(v,out)] -> parse ((\_ -> (string "bc" >>= \_ -> return ('a':"bc"))) v) out) "abcdef"
  parse ((\_ -> (string "bc" >>= \_ -> return ('a':"bc"))) 'a') "bcdef"
  ((\_ -> (string "bc" >>= \_ -> return ('a':"bc"))) 'a') "bcdef"
  (string "bc" >>= \_ -> return ('a':"bc")) "bcdef"
  (string ('b':"c") >>= \_ -> return ('a':"bc")) "bcdef"
  (char 'b' >>= \_ -> string "c" >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) "bcdef"
  (\inp -> case parse p inp of
     [] -> []
     [(v,out)] -> parse ((\_ -> string "c" >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) v) out) "bcdef"
  parse ((\_ -> string "c" >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) 'b') "cdef"
  ((\_ -> string "c" >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) 'b') "cdef"
  (string "c" >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) "cdef"
  (string ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) "cdef"
  ((char 'c' >>= \_ -> string [] >>= \_ -> return ('c':[])) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) "cdef"
  (\inp -> case parse (char 'c') inp of
      [] -> []
      [(v,out)] -> parse ((\_ -> string [] >>= \_ -> return ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) v) out) "cdef"
  parse ((\_ -> string [] >>= \_ -> return ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) 'c') "def"
  ((\_ -> string [] >>= \_ -> return ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) 'c') "def"
  (string [] >>= \_ -> return ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) "def"
  (return [] >>= \_ -> return ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) "def"
  (\inp -> case parse (return []) inp of
      [] -> []
      [(v,out)] -> parse ((\_ -> return ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) v) out) "def"
  parse ((\_ -> return ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) []) "def"
  ((\_ -> return ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) []) "def"
  (return ('c':[]) >>= \_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) "def"
  (\inp -> case parse return "c" inp of
      [] -> []
      [(v,out)] -> parse ((\_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) v) out) "def"
  parse ((\_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) 'c') "def"
  ((\_ -> return ('b':"c") >>= \_ -> return ('a':"bc")) 'c') "def"
  (return ('b':"c") >>= \_ -> return ('a':"bc")) "def"
  (\inp -> case parse (return "bc") inp of
      [] -> []
      [(v,out)] -> parse ((\_ -> return ('a':"bc")) v) out) "def"
  parse ((\_ -> return ('a':"bc")) "bc") "def"
  ((\_ -> return ('a':"bc")) "bc") "def"
  (return ('a':"bc")) "def"
  [("abc","def")]
--}

{--
  parse (string "abc") "abcdef"
  parse (string "abc") "ab1234"
--}

string2 :: String -> Parser String
string2 [] = return []
string2 (x:xs) = char x >>= \_ -> string xs >>= \_ -> return "hoge"

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v -> many p >>= \vs -> return (v:vs)

{--
  parse (many digit) "123abc"
  (many digit) "123abc"
  (many1 digit +++ return []) "123abc"
  (\inp -> case parse (many1 digit) inp of
      [] -> parse (return [])
      [(v,out)] -> [(v,out)]) "123abc"
  (\inp -> case parse (digit >>= \v -> many digit >>= \vs -> return (v:vs)) inp of
      [] -> parse (return [])
      [(v,out)] -> [(v,out)]) "123abc"
  [("123","abc")]
--}

{--
  parse (digit >>= \v -> many digit >>= \vs -> return (v:vs)) "123abc"
  (digit >>= \v -> many digit >>= \vs -> return (v:vs)) "123abc"
  (\inp -> case parse digit inp of
    [] -> []
    [(v,out)] -> parse ((\v -> many digit >>= \vs -> return (v:vs)) v) out) "123abc"
  parse ((\v -> many digit >>= \vs -> return (v:vs)) '1') "23abc"
  ((\v -> many digit >>= \vs -> return (v:vs)) '1') "23abc"
  (many digit >>= \vs -> return ('1':vs)) "23abc"
--}

{--
  (many digit >>= \vs -> return ('1':vs)) "23abc"
  ((many1 digit +++ return []) >>= \vs -> return ('1':vs)) "23abc"
  (\inp -> case parse (many1 digit +++ return []) inp of
      [] -> []
      [(v,out)] -> parse ((\vs -> return ('1':vs)) v) out) "23abc"
  (\inp -> case (many1 digit +++ return []) inp of
      [] -> []
      [(v,out)] -> parse ((\vs -> return ('1':vs)) v) out) "23abc"
--}

{--
  (many1 digit +++ return []) "23abc"
  (\inp -> case parse (many1 digit) inp of
      [] -> parse (return []) inp
      [(v,out)] -> [(v,out)]) "23abc"
--}

{--
  parse (many1 digit) "23abc"
  (many1 digit) "23abc"
  (digit >>= \v -> many digit >>= \vs -> return (v:vs)) "23abc"
  (\inp -> case parse digit inp of
      [] -> []
      [(v,out)] -> parse ((\v -> many digit >>= \vs -> return (v:vs)) v) out) "23abc"
  parse ((\v -> many digit >>= \vs -> return (v:vs)) '2') "3abc"
  ((\v -> many digit >>= \vs -> return (v:vs)) '2') "3abc"
  (many digit >>= \vs -> return ('2':vs)) "3abc"
  (many digit >>= \vs -> return ('2':vs)) "3abc"
  ((many1 digit +++ return []) >>= \vs -> return ('2':vs)) "3abc"
--}

{--
  parse (many digit) "123abc"
  parse (many digit) "abcdef"
  parse (many1 digit) "abcdef"
--}

-- ident ((->) String) [(String,String)]
ident :: Parser String
ident = lower >>= \x -> many alphanum >>= \xs -> return (x:xs)

{--
  (lower >>= \x -> many alphanum >>= \xs -> return (x:xs)) "abc def"
  (\inp -> case parse lower inp of
      [] -> []
      [(v,out)] -> parse ((\x -> many alphanum >>= \xs -> return (x:xs)) v) out) "abc def"
--}

nat :: Parser Int
nat = many1 digit >>= \xs -> return (read xs)

space :: Parser ()
space = many (sat isSpace) >>= \_ -> return ()

space2 :: Parser Int -- count a number of white space
space2 = many (sat isSpace) >>= \x -> return (length x)

token :: Parser a -> Parser a
token p = space >>= \_ -> p >>= \v -> space >>= \_ -> return v

token2 :: Parser a -> Parser a
token2 p = space >>= \_ -> p >>= \v -> return v

identifier :: Parser String
identifier = token ident

{--
  parse identifier "    my132  41fa    ;jfilae*:"
  parse ident "    my132  41fa    ;jfilae*:"
--}

natural :: Parser Int
natural = token nat

{--
  parse natural "    13241fa ;jfilae*:"
  parse nat "    13241fa ;jfilae*:"
--}

natural2 :: Parser Int
natural2 = token2 nat

symbol :: String -> Parser String
symbol xs = token (string xs)

{--
  parse (symbol "abc") "  abcdefa;lLIJf@    deftkf "
--}

-- p.98
-- Since p is already used in p.93, I use p'.
p' :: Parser [Int]
p' = (symbol "[") >>= \_ -> natural >>= \n -> many ((symbol ",") >>= \_ -> natural) >>= \ns -> symbol "]" >>= \_ -> return (n:ns)

{--
  parse p' " [1, 2, 3] "
  parse p' " [1, 2, 3,] "
--}

expr :: Parser Int
expr = term >>= \t -> (symbol "+" >>= \_ -> expr >>= \e -> return (t + e)) +++ return t

term :: Parser Int
term = factor >>= \f -> (symbol "*" >>= \_ -> term >>= \t -> return (f * t)) +++ return f

factor :: Parser Int
factor = (symbol "(" >>= \_ -> expr >>= \e -> (symbol ")") >>= \_ -> return e) +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
  [(n,[] )] -> n
  [(_,out)] -> error ("unused input " ++ out)
  []        -> error "invalid input"

{--
  eval "2*3+4"
  eval "2*(3+4)"
  eval "2 * (3 + 4)"
  eval "2*3-4"
  eval "-1"
--}
