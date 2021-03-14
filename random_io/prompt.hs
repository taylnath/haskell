getInput prompt = do
    putStrLn prompt
    desire <- getLine
    return desire

main = do
    rs <- sequence $ map getInput ["What is your name?", "What is your quest?", "What is your favorite color?"]
    print rs

    
