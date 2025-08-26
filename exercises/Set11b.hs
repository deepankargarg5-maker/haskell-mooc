module Set11b where

import Control.Monad
import Data.List
import Data.IORef
import System.IO

import Mooc.Todo

------------------------------------------------------------------------------

-- Ex 1: Append all strings in list to IORef string
appendAll :: IORef String -> [String] -> IO ()
appendAll ref strs = do
  current <- readIORef ref
  let newVal = current ++ concat strs
  writeIORef ref newVal

------------------------------------------------------------------------------

-- Ex 2: Swap two IORef values
swapIORefs :: IORef a -> IORef a -> IO ()
swapIORefs r1 r2 = do
  v1 <- readIORef r1
  v2 <- readIORef r2
  writeIORef r1 v2
  writeIORef r2 v1

------------------------------------------------------------------------------

-- Ex 3: Run IO (IO a) by first executing the outer, then the inner
doubleCall :: IO (IO a) -> IO a
doubleCall op = do
  inner <- op
  inner

------------------------------------------------------------------------------

-- Ex 4: IO composition, like (.) for IO functions
compose :: (a -> IO b) -> (c -> IO a) -> c -> IO b
compose op1 op2 c = do
  a <- op2 c
  op1 a

------------------------------------------------------------------------------

-- Ex 5: Read all lines from a handle as a list of strings
hFetchLines :: Handle -> IO [String]
hFetchLines h = do
  eof <- hIsEOF h
  if eof
    then return []
    else do
      line <- hGetLine h
      rest <- hFetchLines h
      return (line : rest)

------------------------------------------------------------------------------

-- Ex 6: Get specific line numbers from a handle (1-based indexing)
hSelectLines :: Handle -> [Int] -> IO [String]
hSelectLines h nums = do
  lines <- hFetchLines h
  return [line | (i, line) <- zip [1..] lines, i `elem` nums]

------------------------------------------------------------------------------

-- Ex 7: Drive a pure function loop with IO
interact' :: ((String,st) -> (Bool,String,st)) -> st -> IO st
interact' f state = do
  line <- getLine
  let (cont, output, newState) = f (line, state)
  putStrLn output
  if cont
    then interact' f newState
    else return newState
