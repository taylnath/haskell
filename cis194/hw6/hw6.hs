
------------------------------------------------------------
-- Ex 1
------------------------------------------------------------

-- fibonacci sequence defined by F0 = 0 F1 = 1, Fn = F(n-1) + F(n-2)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

oneMoreFib :: [Integer] -> [Integer]
oneMoreFib [] = [0]
oneMoreFib [0] = [0, 1]
-- oneMoreFib x = x ++ [(x !! (length x - 1)) + (x !! (length x - 2))]
oneMoreFib x = x ++ [last x + last(init x)]

nthFib :: Integer -> Integer
nthFib n = last $ iterate oneMoreFib [] !! (fromIntegral n)

fibs2 :: [Integer]
fibs2 = map nthFib [1..]

------------------------------------------------------------
-- Ex 3
------------------------------------------------------------

-- data type of infinite lists
data Stream a = Cons a (Stream a)

-- convert Stream to infinite list
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : (streamToList xs)

-- convert infinite list to Stream
infiniteListToStream :: [a] -> Stream a
infiniteListToStream (x:xs) = Cons x (infiniteListToStream xs)

-- print Streams by showing first 10 elements
instance Show a => Show (Stream a) where
  show x = show $ take 10 $ streamToList x

------------------------------------------------------------
-- Ex 4
------------------------------------------------------------

-- generate a Stream by repeating x
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- apply function to every elt of stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- generates a Stream from a "seed" of type a, which is the first elements, 
-- and an "unfolding rule" of type a -> a that says how to transform the seed 
-- into the new seed. 
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

------------------------------------------------------------
-- Ex 5
------------------------------------------------------------

