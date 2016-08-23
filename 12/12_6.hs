-- Should fix following functions using the notion of the foldable.

repeatT :: a -> Tree a
repeatT x = Node subT x subT
  where subT = repeatT x

takeT :: Int -> Tree a -> Tree a
takeT 0 _                   = Leaf
takeT n Leaf                = Leaf
takeT n (Node left x right) = Node (takeT (n-1) left) x (takeT (n-1) right)

replicateT :: Int -> a -> Tree a
replicateT n = (takeT n) . repeatT

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show
