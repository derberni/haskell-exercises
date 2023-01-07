{-# LANGUAGE InstanceSigs #-}

-- |
-- Module                  : Lecture3
-- Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
-- SPDX-License-Identifier : MPL-2.0
-- Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
-- Stability               : Stable
-- Portability             : Portable
--
-- Exercises for the Lecture 3 of the Haskell Beginners course.
--
-- In this module you're going to practice standard Haskell typeclasses:
--
--   * Deriving instances
--   * Using typeclasses methods
--   * Implementing instances manually
--   * Becoming friends with Semigroup, Monoid, Foldable and Functor typeclasses!
module Lecture3
  ( Weekday (..),
    toShortString,
    next,
    daysTo,
    Gold (..),
    Reward (..),
    List1 (..),
    Treasure (..),
    appendDiff3,
    apply,
  )
where

-- VVV If you need to import libraries, do it after this line ... VVV

import Data.Monoid (Any (..))

-- ^ ^^ and before this line. Otherwise the test suite might fail  ^^^

-- $setup
-- >>> import Data.Semigroup

-- | Let's define a simple enumeration data type for representing days
-- of the week.
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq, Enum, Bounded)

-- | Write a function that will display only the first three letters
-- of a weekday.
--
-- >>> toShortString Monday
-- "Mon"
toShortString :: Weekday -> String
toShortString = take 3 . show

-- | Write a function that returns next day of the week, following the
-- given day.
--
-- >>> next Monday
-- Tuesday
--
-- ♫ NOTE: Implement this function without pattern matching on every
--  constructor! Use standard typeclasses instead (you may need to derive
--  them first).
--
-- 🕯 HINT: Check 'Enum' and 'Bounded' typeclasses.
--
-- 🆙 Bonus challenge 1: Could you implement this function in a such way
--  that it'll still work even if you change constructor names and their
--  order in the 'Weekday' type?
--
-- 🆙 Bonus challenge 2: Now, could you improve the implementation so it
--  would work for **any** enumeration type in Haskell (e.g. 'Bool',
--  'Ordering') and not just 'Weekday'?
next :: (Eq a, Enum a, Bounded a) => a -> a
next d
  | d == maxBound = minBound
  | otherwise = succ d

-- | Implement a function that calculates number of days from the first
-- weekday to the second.
--
-- >>> daysTo Monday Tuesday
-- 1
-- >>> daysTo Friday Wednesday
-- 5
daysTo :: Weekday -> Weekday -> Int
daysTo from to = mod (fromEnum to - fromEnum from + stride) stride
  where
    stride = fromEnum (maxBound :: Weekday) + 1

{-

In the following block of tasks you need to implement 'Semigroup'
instances for all types and 'Monoid' instances if it's possible to
have a lawful 'Monoid' instance.

-}

newtype Gold = Gold
  { unGold :: Int
  }
  deriving (Show, Eq)

-- | Addition of gold coins.
instance Semigroup Gold where
  (<>) :: Gold -> Gold -> Gold
  (<>) (Gold x) (Gold y) = Gold (x + y)

instance Monoid Gold where
  mempty = Gold 0

-- | A reward for completing a difficult quest says how much gold
-- you'll receive and whether you'll get a special reward.
--
-- If you combine multiple rewards, the final reward will contain a
-- special prize if at least one of the rewards is special.
data Reward = Reward
  { rewardGold :: Gold,
    rewardSpecial :: Bool
  }
  deriving (Show, Eq)

instance Semigroup Reward where
  (<>) :: Reward -> Reward -> Reward
  (<>) (Reward goldA specialA) (Reward goldB specialB) = Reward (goldA <> goldB) (specialA || specialB)

instance Monoid Reward where
  mempty = Reward mempty False

-- | 'List1' is a list that contains at least one element.
data List1 a = List1 a [a]
  deriving (Show, Eq)

-- | This should be list append.
instance Semigroup (List1 a) where
  (<>) (List1 lval l) (List1 kval k) = List1 lval (l ++ kval : k)

-- | Does 'List1' have the 'Monoid' instance? If no then why?
--
-- 'List1' does not adhere to the Monoid laws because of the single Value of a it contains.
--
-- instance Monoid (List1 a) where

-- | When fighting a monster, you can either receive some treasure or
-- don't.
data Treasure a
  = NoTreasure
  | SomeTreasure a
  deriving (Show, Eq)

-- | When you append multiple treasures for fighting multiple
-- monsters, you should get a combined treasure and not just the first
-- (or last one).
--
-- 🕯 HINT: You may need to add additional constraints to this instance
--  declaration.
instance Semigroup a => Semigroup (Treasure a) where
  (<>) NoTreasure t = t
  (<>) t NoTreasure = t
  (<>) (SomeTreasure t) (SomeTreasure u) = SomeTreasure (t <> u)

instance Semigroup a => Monoid (Treasure a) where
  mempty :: Treasure a
  mempty = NoTreasure

-- | Abstractions are less helpful if we can't write functions that
-- use them!
--
-- Implement a polymorphic function that takes three elements and appends
-- together only different elements.
--
-- >>> appendDiff3 [1] [3, 2] [0, 5]
-- [1,3,2,0,5]
-- >>> appendDiff3 [4] [2, 2] [2, 2]
-- [4,2,2]
-- >>> appendDiff3 [1 .. 5] [1 .. 5] [1 .. 5]
-- [1,2,3,4,5]
-- >>> appendDiff3 (Product 2) (Product 3) (Product 3)
-- Product {getProduct = 6}
appendDiff3 :: (Eq a, Semigroup a) => a -> a -> a -> a
appendDiff3 x y z
  | x == y && x == z = x
  | x == y = x <> z
  | x == z = x <> y
  | y == z = x <> y
  | otherwise = x <> y <> z

{-

In the next block of tasks, implement 'Foldable' instances for all
types that can have such an instance.

♫ NOTE: Implement both 'foldr' and 'foldMap' methods. On the one hand,
  'Foldable' is a big typeclass but lets focus on its small part to get
  the main idea. On the other hand, these two methods are quite
  different so it's a good practice.

🕯 HINT: Check kinds of types to see whether it's possible to implement
  an instance of 'Foldable'.

🕯 HINT: If you don't feel comfortable with kinds yet, alternatively
  you can try uncommenting each instance one by one and see the GHC
  error. The compiler will "kindly" tell you if it's impossible to have
  such an instance.

🕯 HINT: Write explicit type signature of methods using InstanceSigs
  (already enabled in this module).

♫ NOTE: Since the instances are commented, the tests are also commented.
  To run tests for your instances, go to the "test/Test/Lecture3.hs" file
  and uncomment all commented tests. But do this only after you
  implement instances! No spoilers :)
-}

-- instance Foldable Weekday where
-- instance Foldable Gold where
-- instance Foldable Reward where
instance Foldable List1 where
  foldr :: (a -> b -> b) -> b -> List1 a -> b
  foldr step initial (List1 val []) = step val initial
  foldr step initial (List1 val xs) = foldr step (step val initial) xs

  foldMap :: Monoid m => (a -> m) -> List1 a -> m
  foldMap mcon (List1 val []) = mcon val
  foldMap mcon (List1 val xs) = foldMap mcon xs <> mcon val

instance Foldable Treasure where
  foldr :: (a -> b -> b) -> b -> Treasure a -> b
  foldr _ initial NoTreasure = initial
  foldr step initial (SomeTreasure t) = step t initial

  foldMap :: Monoid m => (a -> m) -> Treasure a -> m
  foldMap _ NoTreasure = mempty
  foldMap mcon (SomeTreasure t) = mcon t

{-

In the next block of tasks, implement 'Functor' instances for all
types that can have such an instance.

🕯 HINT: At this point, you already know which types can have 'Functor'
  instance and which don't (the same types as for 'Foldable' in this
  case). But comments still mention all types to avoid spoilers ;)
-}

-- instance Functor Weekday where
-- instance Functor Gold where
-- instance Functor Reward where
instance Functor List1 where
  fmap :: (a -> b) -> List1 a -> List1 b
  fmap f (List1 val l) = List1 (f val) (map f l)

instance Functor Treasure where
  fmap :: (a -> b) -> Treasure a -> Treasure b
  fmap _ NoTreasure = NoTreasure
  fmap f (SomeTreasure x) = SomeTreasure (f x)

-- | Functions are first-class values in Haskell. This means that they
-- can be even stored inside other data types as well!
--
-- Now, you have a function inside some 'Functor'. You're a given an
-- element and you need to apply the function inside the 'Functor' to a
-- given element.
--
-- >>> apply 5 (Just (+ 3))
-- Just 8
-- >>> apply 5 Nothing
-- Nothing
-- >>> apply [1 .. 10] (Just (drop 7))
-- Just [8,9,10]
-- >>> apply 5 [(+ 3), (* 4), div 17]
-- [8,20,3]
apply :: Functor f => a -> f (a -> a) -> f a
apply x = fmap ($ x)