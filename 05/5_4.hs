-- factors (p.47) を用いると遅いので，改変したものを用いる．

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors' x) == x]
  where
    factors' i = [j | j <- [1..(i`div`2)], i `mod` j == 0]
