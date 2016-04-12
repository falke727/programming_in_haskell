and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ (concat' xss)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

get :: [a] -> Int -> a  -- !! の代わり
get [] _ = error "index too large"
get (x:_)  0 = x
get (x:xs) n = get xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' x []     = False
elem' x (y:ys)
  |x == y    = True
  |otherwise = elem' x ys
