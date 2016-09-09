import System.IO
import Parser hiding (eval)

-- change the eval function.
eval :: String -> IO ()
eval xs = case parse expr xs of
  [(n,"")]  -> calc (show n)
  [(_,out)] -> display out >>= \_ -> getCh >>= \_ -> calc xs

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]

run :: IO ()
run = cls >>= \_ -> showbox >>= \_ -> clear

cls :: IO ()
cls = putStr "\ESC[2J"

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

showbox :: IO ()
showbox = seqn [writeat (1,y) xs | (y,xs) <- zip [1..13] box]

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = a >>= \_ -> seqn as

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = goto p >>= \_ -> putStr xs

goto :: Pos -> IO ()
goto (x,y) =
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

clear :: IO ()
clear = calc ""

calc :: String -> IO ()
calc xs =
  display xs >>= \_ -> getCh >>= \c ->
  if elem c buttons
  then process c xs
  else beep >>= \_ -> calc xs -- This error is not detected via Parser. So I still use the beep function here.

display :: String -> IO ()
display xs = writeat (3,2) "                  " >>= \_ -> writeat (3,2) (reverse (take 13 (reverse xs)))

getCh :: IO Char
getCh = hSetEcho stdin False >>= \_ -> getChar >>= \c -> hSetEcho stdin True >>= \_ -> return c

buttons :: [Char]
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra    = "QCD \ESC\BS\DEL\n"

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC"    = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n"       = eval xs
  | elem c "cC"        = clear
  | otherwise          = press c xs

beep :: IO ()
beep = putStr "\BEL"

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])
