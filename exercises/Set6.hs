module Set6 where

import Mooc.Todo
import Data.Char (toLower)

------------------------------------------------------------------------------
-- Ex 1: Eq instance for Country

data Country = Finland | Switzerland | Norway
  deriving Show

instance Eq Country where
  Finland == Finland = True
  Switzerland == Switzerland = True
  Norway == Norway = True
  _ == _ = False

------------------------------------------------------------------------------
-- Ex 2: Ord instance for Country with Finland <= Norway <= Switzerland

instance Ord Country where
  compare Finland Finland = EQ
  compare Finland _ = LT
  compare Norway Finland = GT
  compare Norway Norway = EQ
  compare Norway Switzerland = LT
  compare Switzerland Switzerland = EQ
  compare Switzerland _ = GT

  -- Or simpler by mapping to ints:
  -- compare c1 c2 = compare (rank c1) (rank c2)
  --   where rank Finland = 1
  --         rank Norway = 2
  --         rank Switzerland = 3

  (<=) c1 c2 = compare c1 c2 /= GT

  min c1 c2 = if c1 <= c2 then c1 else c2

  max c1 c2 = if c1 >= c2 then c1 else c2

------------------------------------------------------------------------------
-- Ex 3: Eq instance for Name ignoring case

data Name = Name String
  deriving Show

instance Eq Name where
  (Name s1) == (Name s2) =
    map toLower s1 == map toLower s2

------------------------------------------------------------------------------
-- Ex 4: Eq instance for custom List type

data List a = Empty | LNode a (List a)
  deriving Show

instance Eq a => Eq (List a) where
  Empty == Empty = True
  (LNode x xs) == (LNode y ys) = x == y && xs == ys
  _ == _ = False

------------------------------------------------------------------------------
-- Ex 5: Price class and instances for Egg and Milk

class Price a where
  price :: a -> Int

data Egg = ChickenEgg | ChocolateEgg
  deriving Show

data Milk = Milk Int -- litres
  deriving Show

instance Price Egg where
  price ChickenEgg = 20
  price ChocolateEgg = 30

instance Price Milk where
  price (Milk l) = 15 * l

------------------------------------------------------------------------------
-- Ex 6: Price instances for Maybe and lists

instance Price a => Price (Maybe a) where
  price Nothing = 0
  price (Just x) = price x

instance Price a => Price [a] where
  price = sum . map price

------------------------------------------------------------------------------
-- Ex 7: Ord instance for Number

data Number = Finite Integer | Infinite
  deriving (Show, Eq)

instance Ord Number where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

------------------------------------------------------------------------------
-- Ex 8: Eq instance for RationalNumber

data RationalNumber = RationalNumber Integer Integer
  deriving Show

instance Eq RationalNumber where
  (RationalNumber a b) == (RationalNumber c d) = a * d == b * c

------------------------------------------------------------------------------
-- Ex 9: simplify rational numbers using gcd

simplify :: RationalNumber -> RationalNumber
simplify (RationalNumber a b) =
  let g = gcd a b
      a' = a `div` g
      b' = b `div` g
  in RationalNumber a' b'

------------------------------------------------------------------------------
-- Ex 10: Num instance for RationalNumber

instance Num RationalNumber where
  (RationalNumber a b) + (RationalNumber c d) =
    simplify $ RationalNumber (a * d + c * b) (b * d)

  (RationalNumber a b) * (RationalNumber c d) =
    simplify $ RationalNumber (a * c) (b * d)

  abs (RationalNumber a b) = RationalNumber (abs a) b

  signum (RationalNumber a _) =
    RationalNumber (signum a) 1

  fromInteger x = RationalNumber x 1

  negate (RationalNumber a b) = RationalNumber (-a) b

------------------------------------------------------------------------------
-- Ex 11: Addable class and instances

class Addable a where
  zero :: a
  add :: a -> a -> a

instance Addable Integer where
  zero = 0
  add = (+)

instance Addable [a] where
  zero = []
  add = (++)

------------------------------------------------------------------------------
-- Ex 12: Cycle class and instances for Color and Suit

class Cycle a where
  step :: a -> a
  stepMany :: Int -> a -> a
  stepMany 0 x = x
  stepMany n x = stepMany (n - 1) (step x)

data Color = Red | Green | Blue
  deriving (Show, Eq)

data Suit = Club | Spade | Diamond | Heart
  deriving (Show, Eq)

instance Cycle Color where
  step Red = Green
  step Green = Blue
  step Blue = Red

instance Cycle Suit where
  step Club = Spade
  step Spade = Diamond
  step Diamond = Heart
  step Heart = Club
