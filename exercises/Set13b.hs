module Set13b where

import Control.Monad.State
import qualified Data.Set as Set

type Maze = [(String, [String])]
type MazeState = State [String] ()

-- Ex 1: Mark a room as visited by adding it to the visited list
-- if it's not already in it.
visit :: Maze -> String -> MazeState
visit maze room = do
  visited <- get
  if room `elem` visited
    then return ()
    else put (room : visited)

-- Ex 2: Get all directly connected rooms from the current room.
neighbors :: Maze -> String -> [String]
neighbors maze room = maybe [] id (lookup room maze)

-- Ex 3: Visit a room and recursively visit all its neighbors
-- that haven't already been visited.
explore :: Maze -> String -> MazeState
explore maze room = do
  visited <- get
  if room `elem` visited
    then return ()
    else do
      visit maze room
      mapM_ (explore maze) (neighbors maze room)

-- Ex 4: Check whether there is a path between two rooms
-- by performing a full DFS and checking if the target is in the visited list
path :: Maze -> String -> String -> Bool
path maze from to =
  let visited = execState (explore maze from) []
  in to `elem` visited
