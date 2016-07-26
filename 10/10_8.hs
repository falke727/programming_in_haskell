data Maybe' a = Just' a | Nothing' deriving (Show)

data List a = Nil | Cons a (List a) deriving (Show)

(++++) :: List a -> List a -> List a
xs          ++++ Nil = xs
Nil         ++++ ys  = ys
(Cons x xs) ++++ ys  = Cons x (xs ++++ ys)

listMap :: (a -> b) -> List a -> List b
listMap f Nil         = Nil
listMap f (Cons a as) = Cons (f a) (listMap f as)

listConcat :: List (List a) -> List a
listConcat Nil = Nil
listConcat (Cons a Nil) = a
listConcat (Cons a as)  = a ++++ (listConcat as)

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ (concat' xss)

{-
   [[3]]
   Cons (Cons 3 Nil) Nil

   [[3,5]]
   Cons (Cons 3 (Cons 5 Nil)) Nil

   [[2,3], [5,7]]
   Cons (Cons 2 (Cons 3 Nil)) (Cons (Cons 5 (Cons 7 Nil)) Nil)
-}

{-

return :: a -> Maybe' a
(>>=)  :: Maybe' a -> (a -> Maybe' b) -> Maybe' b

return :: a -> List a
(>>=)  :: List a -> (a -> List b) -> List b

-}

instance Monad Maybe' where
  return a = Just' a
  Just' x  >>= f = f x
  Nothing' >>= _ = Nothing'

instance Monad List where
  return a = Cons a Nil
  (Cons a as) >>= f = listConcat (listMap f (Cons a as))
  Nil >>= _ = Nil

-- (Cons 3 (Cons 5 Nil)) >>= (\x -> Cons (x*3) Nil)
