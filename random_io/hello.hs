main = do
    putStrLn "hello, what is your name?"
    name <- getLine
    putStrLn "what is your quest?"
    quest <- getLine
    putStrLn "what is your favorite color?"
    color <- getLine
    if color /= "blue"
        then putStrLn "Oh no!!!"
        else putStrLn ("Hello, " ++ name ++ ".")

