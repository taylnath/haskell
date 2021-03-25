module Golf where

-- skips takes a list and returns a list consisting 
-- of skipping 1 ahead, skipping 2 ahead, etc
chop :: Int -> [a] -> [a]
chop n xs = 
    case (drop n xs) of
        [] -> []
        (y:ys) -> [y] ++ (chop n ys)

skip :: Int -> [a] -> [[a]]
skip n xs 
    | n >= length xs    = []
    | otherwise         = (chop n xs) : (skip (n+1) xs)

skips :: [a] -> [[a]]
skips xs = skip 0 xs

-- Find the local maximum of a list of integers, i.e. 
-- if x < y > z then y is local maximum
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima [x, y] = []
localMaxima (x:y:z:zs)
    | x < y && y > z    = y : (localMaxima (y:z:zs))
    | otherwise         = localMaxima (y:z:zs)

-- add x to n-index element in list
addxToNth :: Integer -> Integer -> [Integer] -> [Integer]
addxToNth _ _ [] = []
addxToNth x 0 (y:ys) = [x + y] ++ ys
addxToNth x n (y:ys) = y : (addxToNth x (n-1) ys)

-- add x to all elements in list
addxToAll :: Integer -> [Integer] -> [Integer]
addxToAll _ [] = []
addxToAll x (y:ys) = [x + y] ++ (addxToAll x ys)

tally :: [Integer] -> [Integer]
tally [] = [0,0,0,0,0,0,0,0,0,0]
tally (x:xs)
    | 0 <= x && x<= 9   = addxToNth x x (tally xs)
    | otherwise         = tally []

-- histBuilder takes a tally of integers
-- and returns a string -- one line of the histogram
histBuilder :: [Integer] -> (String, [Integer])
histBuilder [] = ("\n", [])
histBuilder (x:xs)
    | x > 0     = ("*" ++ (fst $ histBuilder xs), [x - 1] ++ (snd $ histBuilder xs))
    | otherwise = (" " ++ (fst $ histBuilder xs), [x] ++ (snd $ histBuilder xs))

-- histLines calls histBuilder until there are no more 
-- numbers in the tally
-- input is a tally
histLines :: [Integer] -> String
histLines [0,0,0,0,0,0,0,0,0,0] = ""
histLines xs =  (histLines (snd $ histBuilder xs)) ++ (fst $ histBuilder xs)


-- histList :: [Integer] -> [String] -> [String]
-- histList [] [] = ["          "]
-- histList [x] [] = 
--     case 
-- histList (x:xs)
--     | 

histogram :: [Integer] -> String
histogram xs = (histLines $ tally $ xs) ++ "==========\n0123456789"
-- histogram (x:xs) = 
histogram _ = histogram []

simpleHistogram :: [Integer] -> String
simpleHistogram [] = "=\n0"
simpleHistogram (x:xs) = "*\n" ++ (simpleHistogram xs)

sH2 :: [Integer] -> String
sH2 [] = "==\n01"
sH2 (x:xs) = ""

main = do
    -- print $ skips [3]
    -- print $ skips [3, 4]
    -- print $ skips [1,2,3,4,5]
    -- print $ localMaxima [2,9,5,6,1]
    -- print $ localMaxima [2,3,4,1,5]
    -- print $ localMaxima [1,2,3,4,5]
    print $ tally [1,1,1,5]
    putStrLn $ histogram [1,1,1,5]
    -- putStrLn $ simpleHistogram [1,1,1,5]
    -- putStrLn $ histLines [0,0,0,0,2,0,0,0,0,1]