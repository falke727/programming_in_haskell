-- https://gist.github.com/yamamotoj/51a56d01770bd537b2c6 さんのコードそのまま

readLine :: IO String
readLine = read []
     where read xs =
             do c <- getChar
                case c of
                  '\n'   -> return $reverse xs
                  '\DEL' -> case xs of
                    [] -> do putStr "\ESC[2D\ESC[K"
                             read xs
                    (y:ys) -> do putStr "\ESC[3D\ESC[K"
                                 read ys
                  _ -> do read (c:xs)
