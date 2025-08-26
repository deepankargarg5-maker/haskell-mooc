module Set9a where

import Data.Char
import Data.List
import Data.Ord

------------------------------------------------------------------------------
-- Ex 1

workload :: Int -> Int -> String
workload nExercises hoursPerExercise
  | total > 100 = "Holy moly!"
  | total < 10  = "Piece of cake!"
  | otherwise   = "Ok."
  where total = nExercises * hoursPerExercise

------------------------------------------------------------------------------
-- Ex 2

echo :: String -> String
echo "" = ""
echo s = s ++ ", " ++ echo (tail s)

------------------------------------------------------------------------------
-- Ex 3

countValid :: [String] -> Int
countValid = length . filter isValid
  where
    isValid str = length str >= 6 &&
                  (str !! 2 == str !! 4 || str !! 3 == str !! 5)

------------------------------------------------------------------------------
-- Ex 4

repeated :: Eq a => [a] -> Maybe a
repeated (x:y:xs)
  | x == y    = Just x
  | otherwise = repeated (y:xs)
repeated _ = Nothing

------------------------------------------------------------------------------
-- Ex 5

sumSuccess :: [Either String Int] -> Either String Int
sumSuccess xs =
  case rights of
    [] -> Left "no data"
    _  -> Right (sum rights)
  where
    rights = [x | Right x <- xs]

------------------------------------------------------------------------------
-- Ex 6

data Lock = Lock Bool String
  deriving Show

aLock :: Lock
aLock = Lock False "1234"

isOpen :: Lock -> Bool
isOpen (Lock open _) = open

open :: String -> Lock -> Lock
open code (Lock False realCode)
  | code == realCode = Lock True realCode
  | otherwise         = Lock False realCode
open _ lock@(Lock True _) = lock

lock :: Lock -> Lock
lock (Lock True code) = Lock False code
lock l = l

changeCode :: String -> Lock -> Lock
changeCode new (Lock True _) = Lock True new
changeCode _ lock = lock

------------------------------------------------------------------------------
-- Ex 7

data Text = Text String
  deriving Show

instance Eq Text where
  Text s1 == Text s2 = filter (not . isSpace) s1 == filter (not . isSpace) s2

------------------------------------------------------------------------------
-- Ex 8

compose :: (Eq a, Eq b) => [(a,b)] -> [(b,c)] -> [(a,c)]
compose ab bc = [(a,c) | (a,b) <- ab, Just c <- [lookup b bc]]

------------------------------------------------------------------------------
-- Ex 9

type Permutation = [Int]

identity :: Int -> Permutation
identity n = [0 .. n - 1]

multiply :: Permutation -> Permutation -> Permutation
multiply p q = map (\i -> p !! (q !! i)) (identity (length p))

permute :: Permutation -> [a] -> [a]
permute ps xs = map snd . sortBy (comparing fst) $ zip ps xs
