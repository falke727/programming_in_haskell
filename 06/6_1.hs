{-
(*) :: Int -> Int -> Int
m * 0     = 0
m * (n+1) = m + m * n
-}

-- 2 ↑ 3 = 2 * (2 ↑ 2)
--        = 2 * 2 * (2 ↑ 1)
--        = 2 * 2 * 2 * (2 ↑ 0)

pow :: Int -> Int -> Int
pow m 0 = 1
pow m n = m * pow m (n-1)
