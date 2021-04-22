{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

------------------------------------------------------------
-- Ex 1
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap f p = Parser (\s -> fmap (first f) (runParser p s))

------------------------------------------------------------
-- Ex 2
------------------------------------------------------------
--  (<*>) :: f (a -> b) -> f a -> f b
instance Applicative Parser where
  pure x = Parser (\y -> Just (x, ""))
  (<*>) pf p = Parser f
    where 
      f s = case runParser pf s of
        Nothing -> Nothing
        Just (g, t) -> fmap (first g) (runParser p t)

        -- | runParser pf s == Nothing = Nothing
        -- | otherwise = fmap (runParser pf s) (runParser p t)

------------------------------------------------------------
-- Ex 3
------------------------------------------------------------

safeSplit :: [String] -> Maybe (String, String)
safeSplit [] = Nothing
safeSplit (x:xs) = Just (x, unwords xs)

type Name = String

parseName :: Parser Name
parseName = Parser f
  where f s = safeSplit $ words s

parsePhone :: Parser String
parsePhone = Parser f
  where f s = safeSplit $ words s

data Employee = Emp { name :: Name, phone :: String }
  deriving Show

-- expects to see characters 'a' and 'b' then returns them as a pair.
abParser :: Parser (Char, Char)
abParser = (\x y -> (x,y)) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> (char 'a') <*> (char 'b')

-- spaceParser :: Parser String
-- spaceParser = (\x )

intPair :: Parser [Integer]
intPair = (\x y z-> [x,z]) <$> posInt <*> (char ' ') <*> posInt

intSpace :: Parser [Integer]
intSpace = (\x y -> [x]) <$> posInt <*> (char ' ')