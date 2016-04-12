--True ∧ True = True
--_    ∧ _    = False

and' :: Bool -> Bool -> Bool
and' p q = if p then if q then True else False else False
