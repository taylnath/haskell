-- Ex 1: re-implement fun1 and fun2 more idiomatically, 
-- potentially using iterate and takeWhile
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

fun2' :: Integer -> Integer
fun2' 0 = 0
fun2' n = 
    let s = span even (iterate (\x -> div x 2) n) 
    in sum (fst s) -- + fun2' (head $ snd s)

data Tree a = Leaf 
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- TODO: finish insert/foldTree

-- generates a balanced (height of left/right subtrees recursively 
-- differ by at most 1) binary tree from the list of values, using foldr.
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
-- increase height recursively in this next case
insert x (Node h Leaf y Leaf) = Node (h+1) (Node 0 Leaf x Leaf) y Leaf -- height incr
-- insert x (Node h Leaf y r) = Node h (Node 0 Leaf x Leaf) y r
insert x (Node h l y Leaf) = Node h l y (Node 0 Leaf x Leaf) -- no height incr
insert x (Node h (Node hL lL yL rL) y (Node hR lR yR rR))
    | hL <= hR   = Node h (insert x (Node hL lL yL rL)) y (Node hR lR yR rR)
    | otherwise = Node h (Node hL lL yL rL) y (insert x (Node hR lR yR rR))

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

updateHeight :: Tree a -> Tree a
updateHeight Leaf = Leaf
updateHeight (Node h Leaf y Leaf) = Node 0 Leaf y Leaf
updateHeight (Node h l y r) = Node ((max (height (updateHeight l)) (height (updateHeight r))) + 1) (updateHeight l) y (updateHeight r)

foldTree :: [a] -> Tree a
foldTree xs = foldr (\x y -> (updateHeight . (insert x)) y) Leaf xs

nodeVal :: Tree Char -> String
nodeVal Leaf = " "
nodeVal (Node _ _ y _) = [y]

appendxToNth :: Integer -> String -> [String] -> [String]
appendxToNth _ x [] = [x]
appendxToNth 0 x (y:ys) = (y ++ x) : ys
appendxToNth n x (y:ys) = [y] ++ appendxToNth (n-1) x ys

stringListTree :: Integer -> [String] -> Tree Char -> [String]
stringListTree level list Leaf = appendxToNth level "." list
stringListTree level list (Node h l y r) 
    = appendxToNth level [y] (stringListTree (level + 1) (stringListTree (level + 1) list l) r)

makeStringListTree :: Int -> Tree Char -> [String]
makeStringListTree numLevels x = stringListTree 0 (replicate numLevels []) x

lStrip :: String -> String
lStrip "" = ""
lStrip (x:xs)
    | x == ' '  = lStrip xs
    | otherwise = x:xs

treeLines :: Int -> [String] -> String
treeLines _ [] = "\n"
-- treeLines 0 (x:xs) = (lStrip x) ++ "\n"
treeLines n (x:xs) = (replicate n '-') ++ (unwords (map (\y -> (replicate n 'x') ++ [y]) x)) ++ "\n" ++ (treeLines (n-1) xs)

printTree :: Int -> Tree Char -> IO ()
printTree numLevels x = putStr $ treeLines numLevels (makeStringListTree numLevels x)
-- printTree numLevels x = putStr $ unlines $ makeStringListTree numLevels x
-- printTree (Node h l y r) = (replicate h ' ') ++ show(y) ++ "\n"

-- xor returns True iff number of True values is odd.
xor :: [Bool] -> Bool
xor xs = foldr (\x y -> (x || y) && not (x && y)) False xs

-- map implemented as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> (f x) : ys) []

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
    -- mapM (\x -> print (fun2 x == fun2' x)) [-10,1,2,3,0,4,5,6]
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