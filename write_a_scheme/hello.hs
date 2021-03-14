module Main where
import System.Environment

sayHello n x = putStrLn ("hello, " ++ x !! n) >> return x

main = getArgs >>= sayHello 0 >>= sayHello 1
