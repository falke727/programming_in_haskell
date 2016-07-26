data Expr = Val Int | Add Expr Expr | Mult Expr Expr deriving Show

value :: Expr -> Int
value (Val x)    = x
value (Add x y)  = (value x) + (value y)
value (Mult x y) = (value x) * (value y)

-- Add (Val 2) (Mult (Val 3) (Val 5))
-- Add (Add (Val 2) (Val 3)) (Val 4)
-- Mult (Add (Mult (Val 2) (Add (Val 3) (Val 5))) (Val 7)) (Val 11)

type Cont = [Op]
data Op = EVAL Expr | ADD Expr | MULT Expr

eval' :: Expr -> Cont -> Int
eval' (Val  n)   c = exec  c n
eval' (Add  x y) c = eval' x ((ADD  y) : c)
eval' (Mult x y) c = eval' x ((MULT y) : c)

exec :: Cont -> Int -> Int
exec []             n = n
exec ((ADD  n) : c) m = exec c ((eval' n []) + m)
exec ((MULT n) : c) m = exec c ((eval' n []) * m)
--exec ((EVAL y) : c) n = eval' y ((ADD n) : c)

value' :: Expr -> Int
value' e = eval' e []

{-

  value' (Add (Add (Val 2) (Val 3)) (Val 4))

= eval' (Add (Add (Val 2) (Val 3)) (Val 4)) []                { value' を適用 }

= eval' (Add (Val 2) (Val 3)) [(ADD (Val 4)]                  { eval' を適用 }

= eval' (Val2) [(ADD (Val 3)), (ADD (Val 4))]                 { eval' を適用 }

= exec [(ADD (Val 3)), (ADD (Val 4))] 2                       { eval' を適用 }

= exec [(ADD (Val 4))] ((eval' (Val 3) []) + 2)               { exec を適用 }

= exec [] ((eval' (Val 4) []) + ((eval' (Val 3) []) + 2))     { exec を適用 }

= (eval' (Val 4) []) + ((eval' (Val 3) []) + 2)               { exec を適用 }

= (exec [] 4) + ((eval' (Val 3) []) + 2)                      { 左の eval' を適用 }

= 4 + ((eval' (Val 3) []) + 2)                                { exec を適用 }

= 4 + ((exec [] 3) + 2)                                       { eval' を適用 }

= 4 + (3 + 2)                                                 { exec を適用 }

= 4 + 5                                                       { + を適用 }

= 9                                                           { + を適用 }

-}
