module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array
import Data.Ix

-- Ex 1
-- Check if all elements are equal by comparing each element to the head
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

-- Ex 2
-- Check if list has distinct elements by comparing length with nub (removes duplicates)
distinct :: Eq a => [a] -> Bool
distinct xs = length xs == length (nub xs)

-- Ex 3
-- Return the middle (median) of three values
middle :: Ord a => a -> a -> a -> a
middle a b c
  | (a >= b && a <= c) || (a <= b && a >= c) = a
  | (b >= a && b <= c) || (b <= a && b >= c) = b
  | otherwise = c

-- Ex 4
-- Difference between max and min
rangeOf :: (Num a, Ord a) => [a] -> a
rangeOf xs = maximum xs - minimum xs

-- Ex 5
-- Return longest list; if tie, pick one with smallest first element
longest :: (Ord a) => [[a]] -> [a]
longest [] = []
longest (x:xs) = foldl pickLongest x xs
  where
    pickLongest cur cand
      | length cand > length cur = cand
      | length cand == length cur && head cand < head cur = cand
      | otherwise = cur

-- Ex 6
-- Increment values for given key in list of pairs
incrementKey :: (Eq k, Num v) => k -> [(k,v)] -> [(k,v)]
incrementKey key = map (\(k,v) -> if k == key then (k, v + 1) else (k,v))

-- Ex 7
-- Average of Fractional values, convert length to fractional using fromIntegral
average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

-- Ex 8
-- Given a score map and two players, return the one with higher score,
-- ties go to first player
winner :: Map.Map String Int -> String -> String -> String
winner scores p1 p2
  | score1 >= score2 = p1
  | otherwise = p2
  where
    score1 = Map.findWithDefault 0 p1 scores
    score2 = Map.findWithDefault 0 p2 scores

-- Ex 9
-- Compute frequency of each element using Map.alter and foldr
freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int
freqs = foldr (\x m -> Map.alter incr x m) Map.empty
  where
    incr Nothing = Just 1
    incr (Just n) = Just (n + 1)

-- Ex 10
-- Transfer money only if from/to exist, amount positive, and from has enough
transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank
  | amount < 0 = bank
  | not (Map.member from bank) = bank
  | not (Map.member to bank) = bank
  | Map.findWithDefault 0 from bank < amount = bank
  | otherwise = Map.insert to (toVal + amount) $ Map.insert from (fromVal - amount) bank
  where
    fromVal = Map.findWithDefault 0 from bank
    toVal = Map.findWithDefault 0 to bank

-- Ex 11
-- Swap elements at indices i and j in array
swap :: Ix i => i -> i -> Array i a -> Array i a
swap i j arr = arr // [(i, arr ! j), (j, arr ! i)]

-- Ex 12
-- Find index of largest element in array
maxIndex :: (Ix i, Ord a) => Array i a -> i
maxIndex arr = fst $ maximumBy (comparing snd) (assocs arr)
