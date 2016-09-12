import Control.Applicative
import Data.Char
import Data.List.Split
import System.IO
import Parser hiding (eval)

board = [5,4,3,2,1]

showBoard :: IO ()
showBoard = seqn [writeat (1,y) xs | (y,xs) <- zip [1..5] bs] >>= \_ -> putChar '\n'
  where bs = zipWith (++) ["1:","2:","3:","4:","5:"] [(take b (repeat '*')) | b <- board] 

showBoard' :: [Int] -> IO ()
showBoard' ts = cls >>= \_ -> seqn [writeat (1,y) xs | (y,xs) <- zip [1..5] bs] >>= \_ -> putChar '\n'
  where bs = zipWith (++) ["1:","2:","3:","4:","5:"] [(take b (repeat '*')) | b <- ts] 

-- 入力の形式と入力が正しいかを判定し，間違っていたら再入力を促すように改善する必要あり．
-- 例えば，最初の状況で 5行目から2つ取るなどは許されないし，row < 1 || 5 < row 等も許されない．
getPair :: Int -> [Int] -> IO (Int,Int)
getPair c ls = readLine >>= \xs ->
  case filter (/="") (splitOn " " xs) of
    [inp]        -> if inp == "q" then return (-1, -1) else putStrLn "Illegal Input!" >>= \_ -> prompt c >>= \_ -> getPair c ls
    (row:col:[]) ->
      if (r < 1 || 5 < r) || (ls !! (r-1)) < c
      then putStrLn "Illegal Input!" >>= \_ -> prompt c >>= \_ -> getPair c ls
      else return (read row, read col)
      where r = read row
            c = read col
    _            -> putStrLn "Illegal Input!" >>= \_ -> prompt c >>= \_ -> getPair c ls

counter = 1

run :: IO ()
run = cls >>= \_ -> calc' board counter

changeBoard :: [Int] -> (Int,Int) -> [Int]
changeBoard xs (r,c) = (take (r-1) xs) ++ [(xs !! (r-1)) - c] ++ (drop r xs)

calc' :: [Int] -> Int -> IO ()
calc' xs c =
  showBoard' xs >>= \_ -> prompt c >>= \_ -> getPair c xs >>= \(row,col) ->
  if row == -1 then return () else
    case (changeBoard xs (row, col)) of
      [0,0,0,0,0] -> showBoard' (changeBoard xs (row,col)) >>= \_ -> putStrLn $ "Player" ++ p ++ " win!" where p = if c `mod` 2 == 0 then "2" else "1"
      _           -> calc' (changeBoard xs (row,col)) (c+1)

prompt :: Int -> IO ()
prompt n
  | n `mod` 2 == 1 = putStr "P1:"
  | otherwise      = putStr "P2:"

readLine :: IO String
readLine = read []
     where read xs =
             do c <- getChar
                case c of
                  '\n'   -> return $reverse xs
                  '\DEL' -> case xs of
                    [] -> do putStr "\ESC[2D\ESC[K"
                             read xs
                    (y:ys) -> do putStr "\ESC[3D\ESC[K"
                                 read ys
                  _ -> do read (c:xs)

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
