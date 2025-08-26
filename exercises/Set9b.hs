module Set9b where

import Data.List

type Row   = Int
type Col   = Int
type Coord = (Row, Col)
type Size = Int
type Candidate = Coord
type Stack     = [Coord]

--------------------------------------------------------------------------------
-- Ex 1
nextRow :: Coord -> Coord
nextRow (i, _) = (i + 1, 1)

nextCol :: Coord -> Coord
nextCol (i, j) = (i, j + 1)

--------------------------------------------------------------------------------
-- Ex 2
prettyPrint :: Size -> [Coord] -> String
prettyPrint n coords =
  [ if (r, c) `elem` coords then 'Q' else '.'
  | r <- [1..n], c <- [1..n] ] 
  & chunksOf n
  & map (++ "\n")
  & concat

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

--------------------------------------------------------------------------------
-- Ex 3
sameRow :: Coord -> Coord -> Bool
sameRow (i, _) (k, _) = i == k

sameCol :: Coord -> Coord -> Bool
sameCol (_, j) (_, l) = j == l

sameDiag :: Coord -> Coord -> Bool
sameDiag (i, j) (k, l) = i - j == k - l

sameAntidiag :: Coord -> Coord -> Bool
sameAntidiag (i, j) (k, l) = i + j == k + l

--------------------------------------------------------------------------------
-- Ex 4
danger :: Candidate -> Stack -> Bool
danger c = any (\q -> sameRow c q || sameCol c q || sameDiag c q || sameAntidiag c q)

--------------------------------------------------------------------------------
-- Ex 5
prettyPrint2 :: Size -> Stack -> String
prettyPrint2 n coords =
  [ if (r, c) `elem` coords then 'Q'
    else if danger (r, c) coords then '#'
    else '.'
  | r <- [1..n], c <- [1..n] ]
  & chunksOf n
  & map (++ "\n")
  & concat

--------------------------------------------------------------------------------
-- Ex 6
fixFirst :: Size -> Stack -> Maybe Stack
fixFirst _ [] = Nothing
fixFirst n (c@(r, col) : rest)
  | col > n         = Nothing
  | danger c rest   = fixFirst n (nextCol c : rest)
  | otherwise       = Just (c : rest)

--------------------------------------------------------------------------------
-- Ex 7
continue :: Stack -> Stack
continue (c:cs) = nextRow c : c : cs
continue [] = []  -- edge case

backtrack :: Stack -> Stack
backtrack (_:c:cs) = nextCol c : cs
backtrack _ = []  -- edge case

--------------------------------------------------------------------------------
-- Ex 8
step :: Size -> Stack -> Stack
step n s =
  case fixFirst n s of
    Just s' -> continue s'
    Nothing -> backtrack s

--------------------------------------------------------------------------------
-- Ex 9
finish :: Size -> Stack -> Stack
finish n s
  | length s == n + 1 = tail s  -- drop the (n+1)th candidate
  | otherwise = finish n (step n s)

solve :: Size -> Stack
solve n = finish n [(1,1)]
