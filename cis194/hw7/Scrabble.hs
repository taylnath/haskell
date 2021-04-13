module Scrabble where

import Data.Char

data Score = Score Int
  deriving (Eq, Ord, Show)

instance Monoid Score where
  mempty = Score 0
  mappend = (<>)

instance Semigroup Score where
  (<>) (Score n) (Score m) = Score (n + m)

score :: Char -> Score
score x
  | elem c "AEIOULNSTR"   = Score 1
  | elem c "DG"           = Score 2
  | elem c "BCMP"         = Score 3
  | elem c "FHVWY"        = Score 4
  | elem c "K"            = Score 5
  | elem c "JX"           = Score 8
  | elem c "QZ"           = Score 10
  | otherwise             = Score 0
  where
    c = toUpper x

scoreToInt :: Score -> Int
scoreToInt (Score n) = n

scoreString :: String -> Score
scoreString x = foldl (<>) mempty (map score x)