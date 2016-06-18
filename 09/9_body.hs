import System.IO

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
quit = undefined

delete :: String -> IO ()
delete = undefined

eval :: String -> IO ()
eval = undefined

clear :: IO ()
clear = undefined

press :: Char -> String -> IO ()
press = undefined
