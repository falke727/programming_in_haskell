-- last' :: [a] -> a
-- last' [x] = x
-- last' (x:xs) = last' xs

last' :: [a] -> a
last' xs = xs !! (length xs - 1)
