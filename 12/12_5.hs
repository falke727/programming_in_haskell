fibs :: [Integer]
fibs = 0:1: zipWith (+) fibs (tail fibs)

fib :: Int -> Integer
fib n = last (take (n+1) fibs)

firstGT1000 :: Integer
firstGT1000 = y + z
  where
    y = (last . init) xs
    z = last xs
    xs = (takeWhile (< 1000) fibs)
