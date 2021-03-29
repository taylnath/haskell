-- Exercise 2

import Data.List (intercalate)

-- a Tree is either a leaf, or a Node with left/right subtrees
data Tree a = Leaf 
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

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

-- nodeVal :: Tree Char -> String
-- nodeVal Leaf = " "
-- nodeVal (Node _ _ y _) = [y]

appendxToNth :: Int -> String -> [String] -> [String]
appendxToNth _ x [] = [x]
appendxToNth 0 x (y:ys) = (y ++ x) : ys
appendxToNth n x (y:ys) = [y] ++ appendxToNth (n-1) x ys

stringListTree :: Int -> [String] -> Tree Char -> [String]
-- stringListTree level list Leaf = appendxToNth level "." list
stringListTree (-1) list _ = list
stringListTree level list Leaf = appendxToNth level "." (stringListTree (level - 1) (stringListTree (level - 1) list Leaf) Leaf)
stringListTree level list (Node h l y r) 
    = appendxToNth level [y] (stringListTree (level - 1) (stringListTree (level - 1) list l) r)

makeStringListTree :: Int -> Tree Char -> [String]
makeStringListTree numLevels x = stringListTree numLevels (replicate numLevels []) x

-- | Returns n spaces.
spaces :: Int -> String
spaces n = replicate n ' '

-- | Puts n spaces in-between each character of a string
interSpace :: Int -> String -> String
interSpace n s = intercalate "" $ map (\x -> spaces n ++ [x]) s

-- | Uses spaces and interSpace to space a string
spacer :: Int -> Int -> String -> String
spacer _ _ "" = ""
spacer preSpaces interSpaces (x:xs) = spaces preSpaces ++ [x] ++ interSpace interSpaces xs

-- | Places spaces in the appropriate places of a stringListTree
-- | - The bottom row characters have one space in-between, and no margin space. 
-- | - The next row has the margin space equal to the previous in-between space,
-- |   and the in-between space equal to 2n+1, where n is the previous in-between space.
-- | - Takes the stringList reversed (how makeStringListTree produces it)
-- | - Returns reversed stringList
revTreeLines :: Int -> Int -> Int -> [String] -> [String]
revTreeLines _ _ _ [] = []
revTreeLines 0 a b (x:xs) = revTreeLines 1 a b xs   -- ignore first string
revTreeLines level preSpaces interSpaces (x:xs)     -- space this string, then move to the next
    = [spacer preSpaces interSpaces x] ++ revTreeLines (level+1) interSpaces (2*interSpaces+1) xs

-- | Final processing to print a tree.
-- | - use makeStringListTree to get a list of nodes at each level
-- | - use revTreeLines to place spaces in the appropriate places
-- | - then reverse the result, convert to lines, and print
revPrintTree :: Int -> Tree Char -> IO ()
revPrintTree numLevels x = putStr.unlines.reverse.revTreeLines 0 0 1 $ makeStringListTree numLevels x

printTree :: Tree Char -> IO ()
printTree Leaf = putStr ""
-- number of rows is 1 + height to account for the bottom row (level 0)
printTree (Node h l y r) = revPrintTree (fromIntegral (h+1)) (Node h l y r)
