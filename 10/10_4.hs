data Tree = Leaf Int | Node Tree Int Tree deriving (Show)

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

halve :: [a] -> ([a],[a])
halve xs = (take l xs, drop l xs) where l = (length xs) `div` 2

balance :: [Int] -> Tree
balance [] = Leaf 0
balance xs = Node (balance xs1) (head xs2) (balance (tail xs2))
  where
    half = halve xs
    xs1 = fst half
    xs2 = snd half

{-
  Node
   (Node
    (Node
     (Leaf 0)
     1
     (Leaf 0)
    )
    2
    (Node
     (Leaf 0)
     3
     (Leaf 0)
    )
   )
   4
   (Node
    (Node
     (Leaf 0)
     5
     (Leaf 0))
    6
    (Leaf 0)
   )
-}
