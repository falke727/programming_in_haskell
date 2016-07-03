import System.IO
import Parser hiding (eval)

-- p.
echo' :: IO ()
echo' = getChar >>= \c -> putChar '\n' >>= \_ -> putChar c >>= \_ -> putChar '\n'

getLine' :: IO String
getLine' = getChar >>= \x -> if x == '\n'
                             then return []
                             else getLine' >>= \xs -> return (x:xs)

putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = putChar x >>= \_ -> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = putStr' xs >>= \_ -> putChar '\n'

strlen' :: IO ()
strlen' =
  putStr "Enter a string: " >>= \_ -> getLine >>= \xs ->
  putStr "The string has " >>= \_ -> putStr (show (length xs)) >>= \_ ->
  putStrLn " characters."

-- p.109
beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) =
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = goto p >>= \_ -> putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = a >>= \_ -> seqn as

putStr2 xs = seqn [putChar x | x <- xs]

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeat (1,y) xs | (y,xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = writeat (3,2) "                  " >>= \_ -> writeat (3,2) (reverse (take 13 (reverse xs)))

getCh :: IO Char
getCh = hSetEcho stdin False >>= \_ -> getChar >>= \c -> hSetEcho stdin True >>= \_ -> return c

calc :: String -> IO ()
calc xs =
  display xs >>= \_ -> getCh >>= \c ->
  if elem c buttons
  then process c xs
  else beep >>= \_ -> calc xs

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC"    = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n"       = eval xs
  | elem c "cC"        = clear
  | otherwise          = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
  [(n,"")] -> calc (show n)
  _        -> beep >>= \_ -> calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = cls >>= \_ -> showbox >>= \_ -> clear

-- p.114

width :: Int
width = 5

height :: Int
height = 5

{-

(5,5)      (1,5) (2,5) (3,5) (4,5) (5,5)   (1,5)
            /|\   /|\   /|\   /|\   /|\
         + ----------------------------- +
(5,1) <- | (1,1) (2,1) (3,1) (4,1) (5,1) | -> (1,1)
{5,2} <- | (1,2) (2,2) (3,2) (4,2) (5,2) | -> (1,2)
{5,3} <- | (1,3) (2,3) (3,3) (4,3) (5,3) | -> (1,3)
{5,4} <- | (1,4) (2,4) (3,4) (4,4) (5,4) | -> (1,4)
(5,5) <- | (1,5) (2,5) (3,5) (4,5) (5,5) | -> (1,5)
         + ----------------------------- +
            \|/   \|/   \|/   \|/   \|/
(5,5)      (1,1) (2,5) (3,5) (4,5) (5,5)   (1,1)
-}

type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isEmpty b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1), (x+1,y-1),
                          (x-1,y), (x+1,y),
                          (x-1,y+1), (x,y+1), (x+1,y+1)]

-- x mod y = x - y*(floor(x/y))
wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b , elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
{-
births b = [(x,y) | x <- [1..width],
                    y <- [1..height],
                    isEmpty b (x,y),
                    liveneighbs b (x,y) == 3]
-}
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = cls >>= \_ -> showcells b >>= \_ -> wait 5000 >>= \_ -> life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]
