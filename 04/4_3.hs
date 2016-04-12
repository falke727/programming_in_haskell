or0 :: Bool -> Bool -> Bool
or0 True True   = True
or0 True False  = True
or0 False True  = True
or0 False False = False

or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 _     _     = True

or2 :: Bool -> Bool -> Bool
or2 False b = b
or2 True  _ = True
