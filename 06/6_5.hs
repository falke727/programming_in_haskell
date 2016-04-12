halv :: [a] -> ([a],[a])
halv xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

merge :: Ord a => [a] -> [a] -> [a]
--merge xs ys = ?
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  |x <= y    = x : merge xs (y:ys)
  |otherwise = y : merge (x:xs) ys 

msort :: Ord a => [a] -> [a]
-- msort xs = ?
msort []  = []
msort [x] = [x]
msort xs = merge (msort t1) (msort t2)
  where
    t1 = fst $ halv xs
    t2 = snd $ halv xs

merge' :: Ord a => [a] -> [a] -> [a]
--merge xs ys = ?
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs) (y:ys)
  |x <= y    = y : merge' (x:xs) ys
  |otherwise = x : merge' xs (y:ys)

msort' :: Ord a => [a] -> [a]
-- msort' xs = ?
msort' []  = []
msort' [x] = [x]
msort' xs = merge' (msort' t1) (msort' t2)
  where
    t1 = fst $ halv xs
    t2 = snd $ halv xs
