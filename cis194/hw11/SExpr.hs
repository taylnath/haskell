{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParserOfficial
-- import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore $ satisfy $ isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- parseParenNoConsume :: Parser Char
-- parseParenNoConsume = fmap (\(x,y) -> x:y) (char ')') 

-- parse an Atom integer
parseN :: Parser Atom
-- parseN = (\x y -> x) <$> (fmap (\x -> N x) posInt) <*> (char ' ')
parseN = fmap (\x -> N x) posInt

-- parse an Atom identifier
parseI :: Parser Atom
parseI = fmap (\x -> I x) ident

-- parse an Atom
parseAtom :: Parser Atom
parseAtom = parseN <|> parseI

-- parse an Atom as an S Expression
parseA :: Parser SExpr
parseA = fmap (\x -> A x) parseAtom

-- parseSpacedAs :: Parser [ SExpr ]
-- parseSpacedAs = oneOrMore (spaces *> parseA)

parseSomeA :: Parser SExpr
parseSomeA = (\x y z -> y) <$> (char '(') <*> parseA <*> (char ')')

-- parse a parser but in parenthesis, throwing away surrounding spaces
parseInParens :: Parser a -> Parser a
-- parseInParens p = (\x y z -> y) <$> (char '(') <*> (spaces *> p <* spaces) <*> (char ')')
parseInParens p = (\x y z -> y) <$> (char '(') <*> (spaces *> p ) <*> (char ')')

parseComb :: Parser [SExpr] -> Parser SExpr
parseComb = fmap (\x -> Comb x) 

parseSExpr :: Parser SExpr
-- parseSExpr = (spaces *> parseA <* spaces) <|> (parseComb (oneOrMore (parseInParens parseSExpr)))
-- parseSExpr = (spaces *> parseA) <|> (spaces *> (parseComb (parseInParens (oneOrMore parseSExpr))))
parseSExpr = (spaces *> parseA) <|> (parseComb (parseInParens (oneOrMore (spaces *> parseSExpr <* spaces))))

-- parseSExpr :: Parser SExpr
-- parseSExpr = (spaces *> parseA) <|> (oneOrMore expr)
--   where
--     expr = (\x y z -> Comb y) <$> (char '(') <*> parseSExpr <*> (char ')')
