unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (== []) (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold (null) (f . head) (tail)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f

{-

-- chop8 [1..10] = [[1,2,3,4,5,6,7,8],[9,10]]
-- chop8 [1..3]  = [[1,2,3]]

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)


int2bin :: Int -> [Int] 
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin' :: Int -> [Int]
int2bin' = unfold (==0) (`mod` 2) (`div` 2)


                     p       h         t     x

int2bin 4 = unfold (==0) (`mod` 2) (`div` 2) 4

          = (`mod` 2) 4 : unfold (==0) (`mod` 2) (`div` 2) ((`div` 2) 4)

          = (`mod` 2) 4 : unfold (==0) (`mod` 2) (`div` 2) 2

          = (`mod` 2) 4 : 
            (`mod` 2) 2 : unfold (==0) (`mod` 2) (`div` 2) ((`div` 2) 2)

          = (`mod` 2) 4 : 
            (`mod` 2) 2 : unfold (==0) (`mod` 2) (`div` 2) 1

          = (`mod` 2) 4 : 
            (`mod` 2) 2 : 
            (`mod` 2) 1 : unfold (==0) (`mod` 2) (`div` 2) ((`div` 2) 1)

          = (`mod` 2) 4 : 
            (`mod` 2) 2 : 
            (`mod` 2) 1 : unfold (==0) (`mod` 2) (`div` 2) 0

          = (`mod` 2) 4 : 
            (`mod` 2) 2 : 
            (`mod` 2) 1 : []

          = (`mod` 2) 4 : (`mod` 2) 2 : (`mod` 2) 1 : []

          = 0 : 0 : 1 : []

          = [0,0,1]
-}
