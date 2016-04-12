-- definitions of length, drop and init is follows
--
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' n []     = []
drop' n (_:xs) = drop' (n-1) xs

init' :: [a] -> [a]
init' []     = []
init' [_]    = []
init' (x:xs) = x : init' xs
