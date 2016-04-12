-- foldr :: Foldable t => (a -> b -> b) -> b - > t a -> b
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- (Foldable t を リストに読み替え)
--
-- map :: (a -> b) -> [a] -> [b]
-- map f = foldr ? ?
--

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p = foldr ?

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []


-- 因に Agda で書くと下記の様になる．
--
-- open import Data.Nat
-- open import Data.List
-- open import Data.Bool
--
-- map' : {A B : Set} → (A → B) → List A → List B
-- map' f = foldr (λ x xs → f x ∷ xs) []
--
-- filter' : {A : Set} → (A → Bool) → List A → List A
-- filter' p = foldr (λ x xs → if p x then x ∷ xs else xs) []
