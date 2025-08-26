module Set13b where

import Mooc.Todo
import Control.Monad.State
import Data.List (nub)

-- Define the maze graph as adjacency list
maze1 :: [(String, [String])]
maze1 =
  [ ("Entry", ["Corridor 1", "Pit"])
  , ("Corridor 1", ["Dead end", "Corridor 2"])
  , ("Corridor 2", ["Corridor 3"])
  , ("Corridor 3", [])
  , ("Pit", [])
  , ("Dead end", [])
  ]

-- Ex 1: Lookup the neighbours of a room
neighbours :: [(String, [String])] -> String -> [String]
neighbours maze room = case lookup room maze of
  Just ns -> ns
  Nothing -> []

-- Ex 2: Mark a room as visited and extend the visited list
visit :: [(String, [String])] -> String -> State [String] ()
visit maze room = do
  visited <- get
  if room `elem` visited
    then return ()
    else do
      put (room : visited)
      mapM_ (visit maze) (neighbours maze room)

-- Ex 3: Is there a path from room a to room b?
path :: [(String, [String])] -> String -> String -> Bool
path maze from to = to `elem` visited
  where
    visited = execState (visit maze from) []
