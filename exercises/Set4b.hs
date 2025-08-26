module Set4b where

import Mooc.Todo
import Data.Maybe (maybe)

-- Ex 1
-- countHelper increases accumulator if current Maybe is Nothing,
-- otherwise leaves accumulator unchanged.
countNothings :: [Maybe a] -> Int
countNothings xs = foldr countHelper 0 xs

countHelper :: Maybe a -> Int -> Int
countHelper = \mx acc -> maybe (acc + 1) (const acc) mx
-- Explanation:
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- Here, if Nothing, increment acc by 1; if Just _, keep acc.

-- Ex 2
-- maxHelper takes current element and accumulator (max so far), returns max.
myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = foldr maxHelper x xs

maxHelper :: Int -> Int -> Int
maxHelper x acc = if x > acc then x else acc

-- Ex 3
-- slHelper adds current number to sum and increments count
-- slStart is starting tuple (0,0)
sumAndLength :: [Double] -> (Double, Int)
sumAndLength xs = foldr slHelper slStart xs

slStart :: (Double, Int)
slStart = (0.0, 0)

slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper x (sum, len) = (sum + x, len + 1)

-- Ex 4
-- concatHelper prepends current list to accumulator (which is a list)
-- concatStart is empty list
myConcat :: [[a]] -> [a]
myConcat xs = foldr concatHelper concatStart xs

concatStart :: [a]
concatStart = []

concatHelper :: [a] -> [a] -> [a]
concatHelper xs acc = xs ++ acc
-- Since (++) is not disallowed here, we can use it.
-- If not, could implement (++) with recursion, but not needed now.

-- Ex 5
-- largestHelper keeps track of list of largest elements seen so far.
largest :: [Int] -> [Int]
largest xs = foldr largestHelper [] xs

largestHelper :: Int -> [Int] -> [Int]
largestHelper x [] = [x]
largestHelper x acc@(y:_)
  | x > y = [x]
  | x == y = x : acc
  | otherwise = acc

-- Ex 6
-- headHelper returns Just the current element, ignoring accumulator
-- foldr processes from left to right, so first element is picked
myHead :: [a] -> Maybe a
myHead xs = foldr headHelper Nothing xs

headHelper :: a -> Maybe a -> Maybe a
headHelper x _ = Just x

-- Ex 7
-- lastHelper returns accumulator if it's Just something,
-- otherwise wraps current element as Just (the last element seen)
myLast :: [a] -> Maybe a
myLast xs = foldr lastHelper Nothing xs

lastHelper :: a -> Maybe a -> Maybe a
lastHelper x Nothing = Just x
lastHelper _ acc = acc
