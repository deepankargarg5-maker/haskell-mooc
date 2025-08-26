module Set16a where

import Mooc.Todo
import Test.QuickCheck
import Data.List

-- Ex 1: Check if a list is sorted in ascending order
isSorted :: (Show a, Ord a) => [a] -> Property
isSorted xs = property $ and $ zipWith (<=) xs (drop 1 xs)

-- Ex 2: Total frequency count equals input length
sumIsLength :: Show a => [a] -> [(a,Int)] -> Property
sumIsLength input output = property $ length input == sum (map snd output)

-- Wrong example functions
freq1 :: Eq a => [a] -> [(a,Int)]
freq1 [] = []
freq1 [x] = [(x,1)]
freq1 (x:y:xs) = [(x,1),(y,length xs + 1)]

-- Ex 3: Ensure a picked element from input appears in output
inputInOutput :: (Show a, Eq a) => [a] -> [(a,Int)] -> Property
inputInOutput input output =
  forAll (elements input) $ \x ->
    property $ x `elem` map fst output

-- Another example function
freq2 :: Eq a => [a] -> [(a,Int)]
freq2 xs = map (\x -> (x,1)) xs

-- Ex 4: Ensure each output entry reflects true frequency in input
outputInInput :: (Show a, Eq a) => [a] -> [(a,Int)] -> Property
outputInInput input output =
  forAll (elements output) $ \(x, n) ->
    property $ length (filter (== x) input) == n

-- Another example function
freq3 :: Eq a => [a] -> [(a,Int)]
freq3 [] = []
freq3 (x:xs) = [(x,1 + length (filter (==x) xs))]

-- Ex 5: Combined correctness property for a freq function
frequenciesProp :: ([Char] -> [(Char,Int)]) -> NonEmptyList Char -> Property
frequenciesProp freq (NonEmpty input) =
  let output = freq input in
    conjoin [sumIsLength input output,
             inputInOutput input output,
             outputInInput input output]

-- Correct frequency implementation
frequencies :: Eq a => [a] -> [(a,Int)]
frequencies [] = []
frequencies (x:ys) = (x, length xs) : frequencies others
  where (xs,others) = partition (==x) (x:ys)

-- Ex 6: Generator for sorted list of 3–5 Ints between 0–10
genList :: Gen [Int]
genList = do
  n <- choose (3, 5)
  xs <- vectorOf n (choose (0, 10))
  return (sort xs)

-- Ex 7: Arbitrary instances for Arg and Expression
data Arg = Number Int | Variable Char
  deriving (Show, Eq)

data Expression = Plus Arg Arg | Minus Arg Arg
  deriving (Show, Eq)

instance Arbitrary Arg where
  arbitrary = oneof [Number <$> choose (0, 10),
                     Variable <$> elements "abcxyz"]

instance Arbitrary Expression where
  arbitrary = oneof [Plus <$> arbitrary <*> arbitrary,
                     Minus <$> arbitrary <*> arbitrary]
