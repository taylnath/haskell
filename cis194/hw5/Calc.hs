{-# LANGUAGE TypeSynonymInstances #-}
module Calc where

import ExprT
import Parser

-- data ExprT = Lit Integer
--     | Add ExprT ExprT
--     | Mul ExprT ExprT
--   deriving (Show, Eq)

----------------------------------------------------------
-- Ex 1
----------------------------------------------------------

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e g) = (eval e) + (eval g)
eval (Mul e g) = (eval e) * (eval g)


----------------------------------------------------------
-- Ex 2
----------------------------------------------------------

-- two different implementations
evalStr :: String -> Maybe Integer
evalStr s = fmap eval $ parseExp Lit Add Mul s

evalStr' :: String -> Maybe Integer
evalStr' s = let me = parseExp Lit Add Mul s in 
    case me of
        Just e -> Just (eval e)
        Nothing -> Nothing

----------------------------------------------------------
-- Ex 3
----------------------------------------------------------

class (Show e) => Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

instance Expr ExprT where
    lit n = Lit n
    add e g = Add e g
    mul e g = Mul e g

reify :: ExprT -> ExprT
reify = id

----------------------------------------------------------
-- Ex 4
----------------------------------------------------------

instance Expr Integer where
    lit n = n
    add e g = e + g
    mul e g = e * g

instance Expr Bool where
    lit n = n > 0
    add e g = e || g
    mul e g = e && g

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit n = MinMax n
    add (MinMax n) (MinMax m) = MinMax (max n m)
    mul (MinMax n) (MinMax m) = MinMax (min n m)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit n = Mod7 $ mod n 7
    add (Mod7 n) (Mod7 m) = Mod7 (mod (n + m) 7)
    mul (Mod7 n) (Mod7 m) = Mod7 (mod (n * m) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testMod7 = testExp :: Maybe Mod7

----------------------------------------------------------
-- Ex 6
----------------------------------------------------------

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var Integer
  deriving (Show, Eq)

instance HasVars VarExprT where
    var s = Var 1

instance Expr VarExpr where
    lit n = Calc.Lit n :: VarExprT
    add e g = Calc.Add e g :: VarExprT
    mul e g = Calc.Mul e g :: VarExprT

-- class (Show e) => Expr e where
--     lit :: Integer -> e
--     add :: e -> e -> e
--     mul :: e -> e -> e