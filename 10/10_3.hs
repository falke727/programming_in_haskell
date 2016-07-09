data Tree = Leaf Int | Node Tree Int Tree deriving (Show)

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- occurs :: Int -> Tree -> Bool
-- occurs m (Leaf n)     = m == n
-- occurs m (Node l n r) = (occurs m l) || m == n || (occurs m r)

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

-- The Tree defined in this program is sorted.
occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
  | m == n    = True
  | m <  n    = occurs m l
  | otherwise = occurs m r

diffTwo :: Int -> Int -> Bool
diffTwo x y
  | abs (x-y) < 2 = True
  | otherwise     = False

getNumberOfNodeInTree :: Tree -> Int
getNumberOfNodeInTree (Leaf n) = 1
getNumberOfNodeInTree (Node l n r) = (getNumberOfNodeInTree l) + 1 + (getNumberOfNodeInTree r)

balanced :: Tree -> Bool
balanced (Leaf n) = True
balanced (Node l n r) = diffTwo (getNumberOfNodeInTree l) (getNumberOfNodeInTree r) && balanced l && balanced r

notBalancedTree :: Tree
notBalancedTree = Node (Node (Leaf 3) 2 (Node (Leaf 4) 3 (Leaf 4))) 1 (Node (Node (Leaf 4) 3 (Leaf 4)) 2 (Leaf 3))
