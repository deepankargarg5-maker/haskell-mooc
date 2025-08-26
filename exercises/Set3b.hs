{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo

-- Ex 1
buildList :: Int -> Int -> Int -> [Int]
buildList start count end
  | count <= 0 = [end]
  | otherwise = start : buildList start (count - 1) end

-- Ex 2
sums :: Int -> [Int]
sums i = sumsHelper 1 i 0
  where
    sumsHelper current maxSoFar acc
      | current > maxSoFar = []
      | otherwise = let newAcc = acc + current
                    in newAcc : sumsHelper (current + 1) maxSoFar newAcc

-- Ex 3
mylast :: a -> [a] -> a
mylast def [] = def
mylast _ [x] = x
mylast def (_:xs) = mylast def xs

-- Ex 4
indexDefault :: [a] -> Int -> a -> a
indexDefault [] _ def = def
indexDefault (x:xs) i def
  | i < 0 = def
  | i == 0 = x
  | otherwise = indexDefault xs (i - 1) def

-- Ex 5
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs)
  | x <= y = sorted (y:xs)
  | otherwise = False

-- Ex 6
sumsOf :: [Int] -> [Int]
sumsOf [] = []
sumsOf (x:xs) = x : sumsHelper x xs
  where
    sumsHelper _ [] = []
    sumsHelper acc (y:ys) = let newAcc = acc + y
                            in newAcc : sumsHelper newAcc ys

-- Ex 7
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Ex 8
mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum _ currentMax [] = currentMax
mymaximum bigger currentMax (x:xs)
  | bigger x currentMax = mymaximum bigger x xs
  | otherwise = mymaximum bigger currentMax xs

-- Ex 9
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (a:as) (b:bs) = f a b : map2 f as bs

-- Ex 10
maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []
maybeMap f (x:xs) = case f x of
  Just y -> y : maybeMap f xs
  Nothing -> maybeMap f xs
