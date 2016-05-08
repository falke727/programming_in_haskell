import Parser

comment :: Parser ()
comment = token (string "--") >>= \_ -> many (sat (/='\n')) >>= \_ -> item >>= \_ -> space >>= \_ -> return ()
