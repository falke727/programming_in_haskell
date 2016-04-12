-- following codes are in p.76 and p.83

-- sumsqreven :: [Int] -> Int
-- sumsqreven ns = sum (map (^2) (filter even ns))


compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

--sumsqrevenP87 :: [Int] -> Int
--sumsqrevenP87 = compose [sum, map(^2), filter even]

--
-- 問題の答えは，「リストの中身 sum ,(map (^2)), (filter even)
--               の型が揃っておらず compose を適用できない」である．
