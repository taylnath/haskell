-- Ex 1: re-implement fun1 and fun2 more idiomatically, 
-- potentially using iterate and takeWhile
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2)*fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (\x y -> (x - 2) * y) 1 (filter even xs)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)

main = do
    print $ and $ map (\x -> fun1 x == fun1' x) 
        [
            [1,2,3],
            [4,5,6],
            [0,2,4],
            [4,6,8],
            [1,1,4,5,6,7,8],
            [-10,38,5,9,11]
        ]