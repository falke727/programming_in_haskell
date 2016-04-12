-- 問題文を普通に解釈すると，halv が返す値の型は Maybe ([a],[a]).
--  何故ならば，与えられたリストが偶数の場合に hogehoge と言って
--  いるので．
-- 若しくは，要素数が奇数のリストを食わせたら，例外を投げるように
--  定義するべき．
-- 
-- halv :: [a] -> Maybe ([a],[a])
-- halv xs
--   | length xs `mod` 2 == 1 = Nothing
--   | otherwise              = Just (take ((length xs) `div` 2) xs, drop (length xs `div` 2) xs)

halv :: [a] -> ([a],[a])
halv xs = (take ((length xs) `div` 2) xs, drop (length xs `div` 2) xs)

halv' :: [a] -> ([a],[a])
halv' xs
  | length xs `mod` 2 == 1 = undefined
  | otherwise = (take ((length xs) `div` 2) xs, drop (length xs `div` 2) xs)
