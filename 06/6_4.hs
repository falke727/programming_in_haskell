merge :: Ord a => [a] -> [a] -> [a]
--merge xs ys = ?
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  |x <= y    = x : merge xs (y:ys)
  |otherwise = y : merge (x:xs) ys 
