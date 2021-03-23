
-- converts positive integers to a reversed list of digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = (mod n 10) : (toDigitsRev (div n 10))

-- converts positive integers to a list of digits
toDigits    :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

-- double every other digit of list of numbers, starting with second to last
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [n] = [n]
doubleEveryOther ns = 
    doubleEveryOther (init $ init ns) ++ [2 * (last $ init ns)] ++ [last ns]

-- convert a list of numbers to a list of single-digit numbers
-- by writing each number > 10 as its digits
singleDigits :: [Integer] -> [Integer]
singleDigits [] = []
singleDigits (x:xs) = (toDigits x) ++ (singleDigits xs)

testNum n = do 
    print n
    print $ toDigits n
    print $ toDigitsRev n
    print $ doubleEveryOther $ toDigits n
    print $ singleDigits $ doubleEveryOther $ toDigits n

main = do
    testNum 8765
    testNum 0
    testNum 123
