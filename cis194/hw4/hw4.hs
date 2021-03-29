---------------------------------------------------------
-- Ex 1: re-implement fun1 and fun2 more idiomatically, 
-- potentially using iterate and takeWhile
---------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2)*fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (\x y -> (x - 2) * y) 1 (filter even xs)

-- TODO: finish fun2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)

eo1 :: Integer -> Bool
eo1 x = ((even x) || (x == 1)) && (x /= 0)

init' :: [Integer] -> [Integer]
init' [] = []
init' [x] = [x]
init' x = init x

nList :: Integer -> [Integer]
nList 0 = []
nList 1 = []
nList x = 
    let s = span eo1 (iterate (\y -> div y 2) x)
    in (init'.fst) s ++ nList ((head $ snd s) * 3 + 1)

fun2' :: Integer -> Integer
fun2' n = (sum.nList) n

---------------------------------------------------------
-- Ex 2: See foldTree.hs
---------------------------------------------------------

---------------------------------------------------------
-- Ex 3 Part 1:
---------------------------------------------------------

-- xor returns True iff number of True values is odd.
xor :: [Bool] -> Bool
xor xs = foldr (\x y -> (x || y) && not (x && y)) False xs

---------------------------------------------------------
-- Ex 3 Part 2:
---------------------------------------------------------

-- map implemented as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> (f x) : ys) []

---------------------------------------------------------
-- Ex 4
---------------------------------------------------------

sund :: Integer -> Integer -> Integer
sund x y = x + y + 2 * x * y

sundaramNums :: Integer -> [Integer]
sundaramNums n = [(sund x y) | x <- [1..n], y <- [x..n], (sund x y) <= n]

sundaramList :: Integer -> [Integer]
sundaramList n = filter (\x -> not (elem x (sundaramNums n))) [1..n]

-- | Gives the odd prime numbers up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) $ sundaramList n

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
    print $ and $ map (\x -> fun2 x == fun2' x) [10,1,2,3,4,5,6]
    -- mapM (\x -> print (fun2 x == fun2' x)) [10,1,2,3,4,5,6]
    putStr "\nTesting xor:\n"
    mapM (\x -> print (xor x)) 
        [
            [False, True, False],
            [False, True, False, False, True],
            [],
            [True],
            [False]
        ]

    putStr "\nTesting map'\n"
    mapM (\x -> print (map' (\y -> y*2) x))
        [
            [1,2,3],
            [4,5,6]
        ]