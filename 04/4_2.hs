-- a
-- using conditional expression

-- safetail :: [a] -> [a]
-- safetail xs = if null xs then [] else tail xs

-- b
-- using gard

safetail :: [a] -> [a]
safetail xs
  | null xs   = []
  | otherwise = tail xs

-- c
-- using patern match

-- safetail :: [a] -> [a]
-- safetail [] = []
-- safetail (x:xs) = xs
--safetail xs = tail xs
