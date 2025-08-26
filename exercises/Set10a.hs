module Set10a where

import Data.Char
import Data.List

------------------------------------------------------------------------------

-- Ex 1
doublify :: [a] -> [a]
doublify = concatMap (\x -> [x, x])

------------------------------------------------------------------------------

-- Ex 2
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

------------------------------------------------------------------------------

-- Ex 3
deal :: [String] -> [String] -> [(String, String)]
deal players cards = zip cards (cycle players)

------------------------------------------------------------------------------

-- Ex 4
averages :: [Double] -> [Double]
averages = go 0 0
  where
    go _ _ [] = []
    go s n (x:xs) =
      let s' = s + x
          n' = n + 1
      in (s' / n') : go s' n' xs

------------------------------------------------------------------------------

-- Ex 5
alternate :: [a] -> [a] -> a -> [a]
alternate xs ys z = cycle (xs ++ [z] ++ ys ++ [z])

------------------------------------------------------------------------------

-- Ex 6
lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast n _
  | n <= 0 = True
lengthAtLeast _ [] = False
lengthAtLeast n (_:xs) = lengthAtLeast (n - 1) xs

------------------------------------------------------------------------------

-- Ex 7
chunks :: Int -> [a] -> [[a]]
chunks n xs
  | lengthAtLeast n xs = take n xs : chunks n (tail xs)
  | otherwise = []

------------------------------------------------------------------------------

-- Ex 8
newtype IgnoreCase = IgnoreCase String

instance Eq IgnoreCase where
  IgnoreCase s1 == IgnoreCase s2 = map toLower s1 == map toLower s2

ignorecase :: String -> IgnoreCase
ignorecase = IgnoreCase

------------------------------------------------------------------------------

-- Ex 9
data Room = Room String [(String, Room)]

describe :: Room -> String
describe (Room s _) = s

move :: Room -> String -> Maybe Room
move (Room _ directions) direction = lookup direction directions

play :: Room -> [String] -> [String]
play room [] = [describe room]
play room (d:ds) =
  case move room d of
    Nothing -> [describe room]
    Just r -> describe room : play r ds

maze :: Room
maze = maze1
  where
    maze1 = Room "Maze" [("Left", maze2), ("Right", maze3)]
    maze2 = Room "Deeper in the maze" [("Left", maze3), ("Right", maze1)]
    maze3 = Room "Elsewhere in the maze" [("Left", maze1), ("Right", maze2)]
