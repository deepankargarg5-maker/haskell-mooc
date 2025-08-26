module Set12 where

import Data.Functor
import Data.Foldable
import Data.List
import Data.Monoid

import Mooc.Todo

-- Ex 1
incrementAll :: (Functor f, Num n) => f n -> f n
incrementAll = fmap (+1)

-- Ex 2
fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 = fmap . fmap . fmap

-- Ex 3
data Result a = MkResult a | NoResult | Failure String
  deriving Show

instance Functor Result where
  fmap f (MkResult x) = MkResult (f x)
  fmap _ NoResult = NoResult
  fmap _ (Failure s) = Failure s

-- Ex 4
data List a = Empty | LNode a (List a)
  deriving Show

instance Functor List where
  fmap _ Empty = Empty
  fmap f (LNode x xs) = LNode (f x) (fmap f xs)

-- Ex 5
data TwoList a = TwoEmpty | TwoNode a a (TwoList a)
  deriving Show

instance Functor TwoList where
  fmap _ TwoEmpty = TwoEmpty
  fmap f (TwoNode x y rest) = TwoNode (f x) (f y) (fmap f rest)

-- Ex 6
count :: (Eq a, Foldable f) => a -> f a -> Int
count x = foldr (\y acc -> if x == y then acc + 1 else acc) 0

-- Ex 7
inBoth :: (Foldable f, Foldable g, Eq a) => f a -> g a -> [a]
inBoth xs ys = [x | x <- toList xs, x `elem` toList ys]

-- Ex 8
instance Foldable List where
  foldr _ z Empty = z
  foldr f z (LNode x xs) = f x (foldr f z xs)

-- Ex 9
instance Foldable TwoList where
  foldr _ z TwoEmpty = z
  foldr f z (TwoNode x y rest) = f x (f y (foldr f z rest))

-- Ex 10
data Fun a = Fun (Int -> a)

runFun :: Fun a -> Int -> a
runFun (Fun f) x = f x

instance Functor Fun where
  fmap f (Fun g) = Fun (f . g)

-- Ex 11
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

sumTree :: Monoid m => Tree m -> m
sumTree Leaf = mempty
sumTree (Node x l r) = sumTree l <> x <> sumTree r

instance Foldable Tree where
  foldMap f t = sumTree (fmap f t)
