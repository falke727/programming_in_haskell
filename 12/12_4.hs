primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

{-

sieve [2..]
 = sieve 2:[3..]
 = 2: sieve [x | x <- [3..], x `mod` 2 /= 0]

-}


{-

 1. 最初の二つの数を0と1とする．
 2. 次の数は，前の二つの数を足して算出する．
 3. 手順2に戻る．

 hint: use functions zip and tail

-}

fibs :: [Integer]
fibs = 0:1:[]