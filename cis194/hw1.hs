
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
doubleEveryOther xs = 
    doubleEveryOther (init $ init xs) ++ [2 * (last $ init xs)] ++ [last xs]

-- convert a list of numbers to a list of single-digit numbers
-- by writing each number > 10 as its digits
singleDigits :: [Integer] -> [Integer]
singleDigits [] = []
singleDigits (x:xs) = (toDigits x) ++ (singleDigits xs)

-- sums the digits of all numbers in the list
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [n] = n
sumDigits (x:xs) = x + (sumDigits xs)

-- check if the given numbers form a valid credit card according to the rules
validate :: Integer -> Bool
validate n
    | n <= 0    = False
    | otherwise = (mod (sumDigits $ singleDigits $ doubleEveryOther $ toDigits n) 10) == 0

testNum n = do 
    print n
    print $ toDigits n
    print $ toDigitsRev n
    print $ doubleEveryOther $ toDigits n
    print $ singleDigits $ doubleEveryOther $ toDigits n
    print $ sumDigits $ singleDigits $ doubleEveryOther $ toDigits n
    print $ validate n

testValidate = do
    testNum 8765
    testNum 4012888888881881
    testNum 4012888888881882

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)

testHanoi n = do
    print n
    print $ hanoi n "a" "b" "c"

main = do
    testHanoi 0
    testHanoi 1
    testHanoi 2
    testHanoi 3
    testHanoi 4
