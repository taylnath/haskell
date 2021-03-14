import System.IO
import Data.Char

main = do
    putStrLn "What file to read?"
    input <- getLine
    putStrLn "Output name?"
    output <- getLine
    contents <- readFile input
    writeFile output (map toUpper contents)
