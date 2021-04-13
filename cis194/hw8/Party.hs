{-#       OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree

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

-- From Data.Tree:
-- data Tree a = Node {
--   rootLabel :: a, -- label value
--   subForest :: [Tree a] -- zero or more child trees
-- }

-- fold for trees of the above type
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node a []) = f a [e]
treeFold e f (Node a bs) = f a (map (treeFold e f) bs)

-------------------------------------------------------------
-- Ex 3
-------------------------------------------------------------

-- the first guestlist of each tuple is the best possible guest list 
-- with the boss of that subtree. The second is the best possible guest list
-- without the boss of that subtree

-- put the second of the tuple in the first slot, and
-- put the max of the tuple in the second slot
maxSndFlip :: (Ord a) => (a,a) -> (a,a)
maxSndFlip (glBoss, glNoBoss) = (glNoBoss, max glBoss glNoBoss)

collapseGL :: [(GuestList, GuestList)] -> (GuestList, GuestList)
collapseGL [] = (GL [] 0, GL [] 0)
collapseGL glPairs = mconcat $ map maxSndFlip glPairs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- if the employee is no-ones boss, they should go
nextLevel boss [] = (GL [boss] (empFun boss), GL [] 0)
-- if the boss goes, the sub-bosses will have no fun, so boss gets added to glNoBoss
-- if the boss doesn't go, take the maximum between the sub-bosses going or not
nextLevel boss glPairs = (boss `glCons` glNoBoss, glBoss)
  where (glNoBoss, glBoss) = collapseGL glPairs

-------------------------------------------------------------
-- Ex 4
-------------------------------------------------------------

-- get the max of the pair
maxPair :: (Ord a) => (a,a) -> a
maxPair (x,y) = max x y

-- outputs a fun-maximizing guest list
maxFun :: Tree Employee -> GuestList
maxFun t = maxPair $ treeFold (GL [] 0, GL [] 0) nextLevel t
