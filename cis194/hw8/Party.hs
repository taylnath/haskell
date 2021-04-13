{-#       OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee

-------------------------------------------------------------
-- Ex 1
-------------------------------------------------------------

-- Add an employee to the GuestList
-- Assumes the employee is not already in the GuestList
glCons :: Employee -> GuestList -> GuestList
glCons e (GL el f) = GL (e:el) (f + empFun e)

-- data GuestList = GL [Employee] Fun

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

instance Semigroup GuestList where
  (<>) = mappend

-- returns the guest list with higher fun score
moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL e1 f1) g2@(GL e2 f2)
  | f1 >= f2  = g1
  | otherwise = g2


-------------------------------------------------------------
-- Ex 2
-------------------------------------------------------------

-- data Tree a = Node {
--   rootLabel :: a, -- label value
--   subForest :: [Tree a] -- zero or more child trees
-- }

-- fold for trees of the above type
treeFold :: (b -> a -> b) -> b -> Tree a -> b