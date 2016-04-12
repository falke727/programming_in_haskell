repeat' :: a -> [a]
repeat' x = xs where xs = x:xs
--repeat' x = x:repeat' x

{-

repeat' 5 = 5:xs
          = 5:(repeat' 5)
          = 5:5:xs
          = 5:5:(repeat' 5)
          .
          .
          .
          = 

-}


take' :: Int -> [a] -> [a]
take' 0 _ = []
take' (n+1) [] = []
take' (n+1) (x:xs) = x : (take n xs)
