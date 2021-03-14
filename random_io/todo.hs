import System.IO
import Control.Monad

main = do
    putStrLn "What do you need to do?"
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
    putStrLn "To Do:"
    contents <- readFile "todo.txt"
    forM (lines contents) (\x -> do
        putStr "\t-"
        putStrLn x)
