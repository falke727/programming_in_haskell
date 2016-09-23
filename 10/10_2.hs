{-# LANGUAGE GADTs #-}

data Tree where
  Leaf :: Int -> Tree
  Node :: Tree -> Int -> Tree -> Tree
  deriving (Show)

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- p.124
occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
  | m == n    = True
  | m <  n    = occurs m l
  | otherwise = occurs m r

occurs' :: Int -> Tree -> Bool
occurs' m (Leaf n) = m == n
occurs' m (Node l n r)
  = case compare m n of
  LT -> occurs' m l
  EQ -> True
  _  -> occurs' m r
