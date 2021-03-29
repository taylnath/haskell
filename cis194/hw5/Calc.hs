module Calc where

import ExprT

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