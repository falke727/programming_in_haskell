import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

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

transmit :: String -> String
transmit = decode . channel . encode

-- transmit "high-order functions are easy"

channel :: [Bit] -> [Bit]
channel = id


badTransmit :: String -> String
badTransmit = pdecode . badChannel . pencode

badChannel :: [Bit] -> [Bit]
badChannel = tail
