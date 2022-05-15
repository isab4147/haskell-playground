module Hello where


hello :: String -> IO ()
hello name = putStrLn $ "Hello, " ++ name ++ "!"
