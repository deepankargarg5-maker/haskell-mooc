module Set9b where

import Mooc.Todo
import Data.List
import Data.Function ((&))  -- FIX: import (&)

type Row   = Int
type Col   = Int
type Coord = (Row, Col)
type Size = Int
type Candidate = Coord
type Stack = [Coord]

-- Ex 1
nextRow :: Coord -> Coord
nextRow (i, _) = (i + 1, 1)

nextCol :: Coord -> Coord
nextCol (i, j) = (i, j + 1)

-- Ex 2
prettyPrint :: Size -> [Coord] -> String
prettyPrint n queens =
  [ if (i, j) `elem` queens then 'Q' else '.' | i <- [1..n], j <- [1..n] ]
  & chunksOf n
  & map (++ "\n")
  & concat

-- Ex 3
sameRow :: Coord -> Coord -> Bool
sameRow (i, _) (k, _) = i == k

sameCol :: Coord -> Coord -> Bool
sameCol (_, j) (_, l) = j == l

sameDiag :: Coord -> Coord -> Bool
sameDiag (i, j) (k, l) = i - j == k - l

sameAntidiag :: Coord -> Coord -> Bool
sameAntidiag (i, j) (k, l) = i + j == k + l

-- Ex 4
danger :: Candidate -> Stack -> Bool
danger c = any (\q -> sameRow c q || sameCol c q || sameDiag c q || sameAntidiag c q)

-- Ex 5
prettyPrint2 :: Size -> Stack -> String
prettyPrint2 n queens =
  [ if (i,j) `elem` queens then 'Q'
    else if danger (i,j) queens then '#'
    else '.' | i <- [1..n], j <- [1..n] ]
  & chunksOf n
  & map (++ "\n")
  & concat

-- Ex 6
fixFirst :: Size -> Stack -> Maybe Stack
fixFirst n [] = Nothing
fixFirst n (q:qs)
  | snd q > n = Nothing
  | danger q qs = fixFirst n (nextCol q : qs)
  | otherwise = Just (q:qs)

-- Ex 7
continue :: Stack -> Stack
continue (q:qs) = nextRow q : q : qs
continue [] = []

backtrack :: Stack -> Stack
backtrack (_:q:qs) = nextCol q : qs
backtrack _ = []

-- Ex 8
step :: Size -> Stack -> Stack
step n s = case fixFirst n s of
  Nothing -> backtrack s
  Just s' -> continue s'

-- Ex 9
finish :: Size -> Stack -> Stack
finish n s
  | length s == n + 1 = tail s
  | otherwise = finish n (step n s)

solve :: Size -> Stack
solve n = finish n [(1,1)]


-- Helper: chunksOf from Data.List.Split (redefined locally)
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest
