-- [f x | x <- xs , p x] :: [a] -> (a -> b) -> (a -> Bool) -> [b]

useHighFunc :: [a] -> (a -> b) -> (a -> Bool) -> [b]
useHighFunc xs f p = map f (filter p xs)
