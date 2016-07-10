data Nat = Zero | Suc Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero    n = n
add (Suc m) n = Suc (add m n)

-- (n+1) * m = n * m + m

mult :: Nat -> Nat -> Nat
mult Zero    _ = Zero
mult (Suc m) n = add (mult m n) n
