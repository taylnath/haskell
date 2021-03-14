import Data.Char

p x = putStrLn $ "Hello, " ++ (map toUpper x)

main = do
    putStrLn "What is your name?"
    getLine >>= p
