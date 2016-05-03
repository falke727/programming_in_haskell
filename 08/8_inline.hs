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
string (x:xs) = char x >>= \_ -> string xs >>= \imp -> return (x:xs)

{--
  parse (string "abc") "abcdef"
  parse (string "abc") "ab1234"
--}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v -> many p >>= \vs -> return (v:vs)

{--
  parse (many digit) "123abc"
  parse (many digit) "abcdef"
  parse (many1 digit) "abcdef"
--}
