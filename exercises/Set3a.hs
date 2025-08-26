module Set3a where

import Data.Char
import Data.Either
import Data.List

-- Ex 1
maxBy :: (a -> Int) -> a -> a -> a
maxBy measure a b
  | measure a >= measure b = a
  | otherwise = b

-- Ex 2
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

-- Ex 3
mapMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mapMaybe2 _ Nothing _ = Nothing
mapMaybe2 _ _ Nothing = Nothing
mapMaybe2 f (Just x) (Just y) = Just (f x y)

-- Ex 4
firstHalf :: String -> String
firstHalf s = take ((length s + 1) `div` 2) s

palindrome :: String -> Bool
palindrome s = s == reverse s

palindromeHalfs :: [String] -> [String]
palindromeHalfs xs = map firstHalf (filter palindrome xs)

-- Ex 5
capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : xs

capitalize :: String -> String
capitalize = unwords . map capitalizeFirst . words

-- Ex 6
powers :: Int -> Int -> [Int]
powers k max = takeWhile (<= max) $ iterate (*k) 1

-- Ex 7
while :: (a -> Bool) -> (a -> a) -> a -> a
while check update value
  | check value = while check update (update value)
  | otherwise = value

-- Ex 8
whileRight :: (a -> Either b a) -> a -> b
whileRight check x = case check x of
  Left b -> b
  Right a -> whileRight check a

-- Ex 9
joinToLength :: Int -> [String] -> [String]
joinToLength n xs = [a ++ b | a <- xs, b <- xs, length (a ++ b) == n]

-- Ex 10
(+|+) :: [a] -> [a] -> [a]
xs +|+ ys = take 1 xs ++ take 1 ys

-- Ex 11
sumRights :: [Either a Int] -> Int
sumRights = sum . map (either (const 0) id)

-- Ex 12
multiCompose :: [a -> a] -> a -> a
multiCompose = foldr (.) id

-- Ex 13
multiApp :: ([b] -> c) -> [a -> b] -> a -> c
multiApp f gs x = f (map ($ x) gs)

-- Ex 14
interpreter :: [String] -> [String]
interpreter = go 0 0
  where
    go _ _ [] = []
    go x y (cmd:cmds) = case cmd of
      "up" -> go x (y+1) cmds
      "down" -> go x (y-1) cmds
      "left" -> go (x-1) y cmds
      "right" -> go (x+1) y cmds
      "printX" -> show x : go x y cmds
      "printY" -> show y : go x y cmds
      _ -> go x y cmds  -- ignore unknown commands
