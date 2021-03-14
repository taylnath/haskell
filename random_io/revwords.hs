main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ rev line
            main

rev = unwords.map reverse.words
