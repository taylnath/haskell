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

fun2' :: Integer -> Integer
fun2' 0 = 0
fun2' n = 
    let s = span even (iterate (\x -> div x 2) n) 
    in sum (fst s) -- + fun2' (head $ snd s)

data Tree a = Leaf 
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- generates a balanced (height of left/right subtrees recursively 
-- differ by at most 1) binary tree from the list of values, using foldr.
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h Leaf y Leaf) = Node (h+1) (Node 0 Leaf x Leaf) y Leaf
insert x (Node h Leaf y r) = Node h (Node 0 Leaf x Leaf) y r
insert x (Node h l y Leaf) = Node h l y (Node 0 Leaf x Leaf)
insert x (Node h (Node hL lL yL rL) y (Node hR lR yR rR))
    | hL < hR   = Node h (insert x (Node hL lL yL rL)) y (Node hR lR yR rR)
    | otherwise = Node h (Node hL lL yL rL) y (insert x (Node hR lR yR rR))


foldTree :: [a] -> Tree a
foldTree xs = foldr (\x y -> Leaf) Leaf xs

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
    mapM (\x -> print (fun2 x == fun2' x)) [-10,1,2,3,0,4,5,6]