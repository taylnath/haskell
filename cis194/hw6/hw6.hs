
------------------------------------------------------------
-- Ex 1
------------------------------------------------------------

-- | fibonacci sequence defined by F0 = 0 F1 = 1, Fn = F(n-1) + F(n-2)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

oneMoreFib :: [Integer] -> [Integer]
oneMoreFib [] = [0]
oneMoreFib [0] = [0, 1]
oneMoreFib x = x ++ [(x !! (length x - 1)) + (x !! (length x - 2))]

recursionFib :: [Integer] -> [Integer]
recursionFib [] = OneMoreFib []
recursionFib x = OneMoreFib x 

fibs2 :: [Integer]
fibs2 = OneMoreFib [] ++ OneMoreFib
