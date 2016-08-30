import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0
-- bin2int bits = sum [w * b | (w, b) <- zip weights bits]
--  where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)


encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- add a parity bit to the head
pencode :: String -> [Bit]
pencode xs = p : exs
  where
    exs = encode xs
    p   = (sum [n | n <- exs, n == 1]) `mod` 2


chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)


decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

pdecode :: [Bit] -> String
pdecode []     = []
pdecode (x:xs) = if p == x then decode xs else error "parity error!"
  where
    p = (sum [n | n <- xs, n == 1]) `mod` 2

-- pdecode [1,1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- transmit "high-order functions are difficult"

{-

  (1*a) + (2*b) + (4*c) + (8*d)

= a + (2*b) + (4*c) + (8*d)

= a + 2*(b+(2*c)+(4*d))

= a + 2*(b+2*(c+2*d))

= a + 2*(b+2*(c+2*(d+0)))

-}

{-

foldr (+) v [x0,x1,...,xn] = x1+(x2+(..(xn-1 + (xn + v))..))

foldr :: (a -> b -> b) -> a -> [b] -> a
foldr _ v []     = v
foldr f v (x:xs) = f x (foldr f v xs)

-}

{-
  foldr (\x y -> x + 2 * y) 0 [1,1,0,1]

= (\x y -> x + 2 * y) 1 (foldr (\x y -> x + 2 * y) 0 [1,0,1])

= (\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 1 (foldr (\x y -> x + 2 * y) 0 [0,1]))

= (\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 0 ((\x y -> x + 2 * y) 1 (foldr (\x y -> x + 2 * y) 0 []))))

= (\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 0 ((\x y -> x + 2 * y) 1 0)))

= (\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 0 (1 + 2 * 0)))

= (\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 0 1))

= (\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 1 (0 + 2 * 1))

= (\x y -> x + 2 * y) 1 ((\x y -> x + 2 * y) 1 2)

= (\x y -> x + 2 * y) 1 (1 + 2 * 2)

= (\x y -> x + 2 * y) 1 5

= 1 + 2 * 5

= 11

-}
