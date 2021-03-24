module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
skips [x, y] = [[x,y], [y]]
skips [x, y, z] = [[x,y,z], [y], [z]]

-- Find the local maximum of a list of integers, i.e. 
-- if x < y > z then y is local maximum
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima [x, y] = []
localMaxima (x:y:z:zs)
    | x < y && y > z    = y : (localMaxima (y:z:zs))
    | otherwise         = localMaxima (y:z:zs)

histogram :: [Integer] -> String
histogram [] = "==========\n0123456789"
-- histogram (x:xs) = 
histogram _ = histogram []

simpleHistogram :: [Integer] -> String
simpleHistogram [] = "=\n0"
simpleHistogram (x:xs) = "*\n" ++ (simpleHistogram xs)

main = do
    print $ skips [3]
    print $ skips [3, 4]
    print $ localMaxima [2,9,5,6,1]
    print $ localMaxima [2,3,4,1,5]
    print $ localMaxima [1,2,3,4,5]
    putStrLn $ histogram [1,1,1,5]
    putStrLn $ simpleHistogram [1,1,1,5]