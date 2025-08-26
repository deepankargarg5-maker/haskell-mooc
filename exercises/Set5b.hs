module Set5b where

import Mooc.Todo

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

-- Ex 1
valAtRoot :: Tree a -> Maybe a
valAtRoot Empty        = Nothing
valAtRoot (Node v _ _) = Just v

-- Ex 2
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

-- Ex 3
treeMax :: Tree Int -> Int
treeMax Empty = 0
treeMax (Node v left right) = maximum [v, treeMax left, treeMax right]

-- Ex 4
allValues :: (a -> Bool) -> Tree a -> Bool
allValues _ Empty = True
allValues cond (Node v left right) = cond v && allValues cond left && allValues cond right

-- Ex 5
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node v left right) = Node (f v) (mapTree f left) (mapTree f right)

-- Ex 6
cull :: Eq a => a -> Tree a -> Tree a
cull _ Empty = Empty
cull val (Node v left right)
  | v == val  = Empty
  | otherwise = Node v (cull val left) (cull val right)

-- Ex 7
isOrdered :: Ord a => Tree a -> Bool
isOrdered Empty = True
isOrdered (Node v left right) =
  allValues (< v) left &&
  allValues (> v) right &&
  isOrdered left &&
  isOrdered right

data Step = StepL | StepR
  deriving (Show, Eq)

-- Ex 8
walk :: [Step] -> Tree a -> Maybe a
walk [] Empty = Nothing
walk [] (Node v _ _) = Just v
walk (StepL:steps) (Node _ left _) = walk steps left
walk (StepR:steps) (Node _ _ right) = walk steps right
walk _ Empty = Nothing

-- Ex 9
set :: [Step] -> a -> Tree a -> Tree a
set [] val Empty = Empty
set [] val (Node _ left right) = Node val left right
set (StepL:steps) val Empty = Empty
set (StepL:steps) val (Node v left right) = Node v (set steps val left) right
set (StepR:steps) val Empty = Empty
set (StepR:steps) val (Node v left right) = Node v left (set steps val right)

-- Ex 10
search :: Eq a => a -> Tree a -> Maybe [Step]
search _ Empty = Nothing
search val (Node v left right)
  | val == v  = Just []
  | otherwise = case search val left of
      Just steps -> Just (StepL : steps)
      Nothing -> case search val right of
        Just steps -> Just (StepR : steps)
        Nothing -> Nothing
