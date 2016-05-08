import Parser

int :: Parser Int
int = (token (char '-') >>= \s -> many1 digit >>= \xs -> space >>= \_ -> return (read (s:xs))) +++ natural
