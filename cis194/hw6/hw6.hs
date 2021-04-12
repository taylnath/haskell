{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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
  show x = show $ take 20 $ streamToList x

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

-- stream of natural numbers
nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

-- stream of positive natural numbers
posnats :: Stream Integer
posnats = streamFromSeed (\x -> x + 1) 1


-- function that returns the largest power of 2 that evenly divides n
-- (meant for positive integers only)
fruler :: Integer -> Integer
fruler x
  | odd x = 0
  | otherwise = 1 + fruler (div x 2)

-- stream of the --ruler function-- 
-- 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
-- where the n-th element in the stream (assuming first elt is n=1)
-- is the largest power of 2 which evenly divides n
ruler :: Stream Integer
ruler = streamMap fruler posnats

------------------------------------------------------------
-- Ex 6
------------------------------------------------------------

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate s = streamMap (\n -> (-n)) s
  (+) (Cons n ns) (Cons m ms) = Cons (n + m) ((+) ns ms)
  (*) (Cons n ns) (Cons m ms) = Cons (n * m) ((streamMap (\k -> k * n) ms) + (ns * (Cons m ms)))

instance Fractional (Stream Integer) where
  (/) (Cons n ns) (Cons m ms) = Cons (div n m) (streamMap (\k -> div k m) (ns - ((/) (Cons n ns) (Cons m ms)) * ms))
  -- (/) (Cons n ns) (Cons m ms) = Cons (div n m) 

-- define Fibonacci sequence as the coefficients of the infinite series 
-- x / (1 - x - x^2)
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

------------------------------------------------------------
-- Ex 7
------------------------------------------------------------

-- Data type of 2x2 matrices of integers (a b // c d)
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a b c d) (Matrix e f g h) = Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

instance Show Matrix where
  show (Matrix a b c d) = show a ++ show b ++ "\n" ++ show c ++ show d

matrixF :: Matrix
matrixF = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = 
  let (Matrix a b c d) = matrixF ^ n
  in b

fib5 :: [Integer]
fib5 = map fib4 [0..]