-- following code is in p.8
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

qsortG :: Ord a => [a] -> [a]
qsortG []     = []
qsortG (x:xs) = qsortG larger ++ [x] ++ qsortG smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]
