{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

----------------------------------------------------------
-- Ex 1
----------------------------------------------------------

-- | get tag from a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single a b) = a
tag (Append a b c) = a

-- | Concatenate two joinlists, "multiplying" the monoid tags together
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x y = Append (tag x <> tag y) x y

----------------------------------------------------------
-- Ex 2
----------------------------------------------------------

(!!?) :: [a] -> Int -> Maybe a
[]      !!? _         = Nothing
_       !!? i | i < 0 = Nothing
(x:xs)  !!? 0         = Just x
(x:xs)  !!? i         = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- | Finds the JoinList element at the specified index
-- | i.e. index after we convert JoinList to a list
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                = Nothing
indexJ i x | i < 0            = Nothing
indexJ 0 (Single b a)         = Just a
indexJ i (Single b a)         = Nothing
indexJ i (Append m l r) 
  | i < (getSize.size.tag) l  = indexJ i l
  | otherwise                 = indexJ (i - (getSize.size.tag) l) r

-- | Drops the first i elements of the JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i x | i <= 0 = x
dropJ i (Single b a) | i > 0 = Empty
dropJ i (Append m l r)
  | i >= lSize = dropJ (i - lSize) r
  | otherwise = (dropJ i l) +++ r
  where 
    lSize = (getSize.size.tag) l

-- | Returns the first n elements of a JoinList
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i x | i <= 0 = Empty
takeJ i s@(Single b a) | i > 0 = s
takeJ i (Append m l r)
  | i >= lSize = l +++ takeJ (i - lSize) r
  | otherwise  = takeJ i l
  where
    lSize = (getSize.size.tag) l

-- For testing
ss1 x = Single (Size 1) x
hello = map ss1 "hello"
w = foldl (+++) Empty hello

---------------------------------------------------------------
-- Ex 3
---------------------------------------------------------------

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

---------------------------------------------------------------
-- Ex 4
---------------------------------------------------------------

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single m a) = a
  toString (Append m l r) = toString l ++ toString r

  fromString "" = Empty
  fromString s = foldl (+++) Empty $ map fromLine (lines s)
    where
      fromLine = \x -> Single (scoreString x, 1) x

  line = indexJ

  replaceLine i s b = takeJ i b +++ fromString s +++ dropJ (i+1) b

  numLines b = getSize . size . snd . tag $ b

  value b = scoreToInt . fst . tag $ b

