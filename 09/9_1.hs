import System.IO

getCh :: IO Char
getCh = hSetEcho stdin False >>= \_ -> getChar >>= \c -> hSetEcho stdin True >>= \_ -> return c

readLine :: IO String
readLine = undefined

--delete :: String -> IO ()
--delete "" = undefined

-- '\DEL'
-- "\ESC[1D"
