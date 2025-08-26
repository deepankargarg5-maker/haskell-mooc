module Set9a where

import Data.Char
import Data.List
import Data.Ord

-- Ex 1
workload :: Int -> Int -> String
workload nExercises hoursPerExercise
  | total > 100 = "Holy moly!"
  | total < 10  = "Piece of cake!"
  | otherwise   = "Ok."
  where total = nExercises * hoursPerExercise

-- Ex 2
echo :: String -> String
echo "" = ""
echo s@(c:cs) = s ++ ", " ++ echo cs

-- Ex 3
countValid :: [String] -> Int
countValid = length . filter isValid
  where
    isValid s = length s >= 6 &&
                (s !! 2 == s !! 4 || s !! 3 == s !! 5)

-- Ex 4
repeated :: Eq a => [a] -> Maybe a
repeated (x:y:rest)
  | x == y    = Just x
  | otherwise = repeated (y:rest)
repeated _ = Nothing

-- Ex 5
sumSuccess :: [Either String Int] -> Either String Int
sumSuccess xs =
  case rights of
    [] -> Left "no data"
    _  -> Right (sum rights)
  where
    rights = [x | Right x <- xs]

-- Ex 6
data Lock = Locked String | Opened String
  deriving Show

aLock :: Lock
aLock = Locked "1234"

isOpen :: Lock -> Bool
isOpen (Opened _) = True
isOpen _          = False

open :: String -> Lock -> Lock
open attempt (Locked code)
  | attempt == code = Opened code
  | otherwise       = Locked code
open _ l@(Opened _) = l

lock :: Lock -> Lock
lock (Opened code) = Locked code
lock l@(Locked _)  = l

changeCode :: String -> Lock -> Lock
changeCode new (Opened _)  = Opened new
changeCode _ l@(Locked _)  = l

-- Ex 7
data Text = Text String
  deriving Show

instance Eq Text where
  Text s1 == Text s2 = filter (not . isSpace) s1 == filter (not . isSpace) s2

-- Ex 8
compose :: (Eq a, Eq b) => [(a,b)] -> [(b,c)] -> [(a,c)]
compose f g = [(x, z) | (x, y) <- f, Just z <- [lookup y g]]

-- Ex 9
type Permutation = [Int]

identity :: Int -> Permutation
identity n = [0 .. n - 1]

permute :: Permutation -> [a] -> [a]
permute idxs xs = map snd . sortBy (comparing fst) $ zip idxs xs
