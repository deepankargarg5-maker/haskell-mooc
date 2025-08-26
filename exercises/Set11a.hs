module Set11a where

import Control.Monad
import Data.List
import System.IO

import Mooc.Todo

------------------------------------------------------------------------------

-- Ex 1: Print two lines
hello :: IO ()
hello = do
  putStrLn "HELLO"
  putStrLn "WORLD"

------------------------------------------------------------------------------

-- Ex 2: Greet with "HELLO name"
greet :: String -> IO ()
greet name = putStrLn ("HELLO " ++ name)

------------------------------------------------------------------------------

-- Ex 3: Read a name, then greet
greet2 :: IO ()
greet2 = do
  name <- getLine
  greet name

------------------------------------------------------------------------------

-- Ex 4: Read n words and return them sorted
readWords :: Int -> IO [String]
readWords n = do
  words <- replicateM n getLine
  return (sort words)

------------------------------------------------------------------------------

-- Ex 5: Read lines until predicate returns True
readUntil :: (String -> Bool) -> IO [String]
readUntil f = do
  line <- getLine
  if f line
    then return []
    else do
      rest <- readUntil f
      return (line : rest)

------------------------------------------------------------------------------

-- Ex 6: Print numbers from n down to 0
countdownPrint :: Int -> IO ()
countdownPrint n
  | n < 0     = return ()
  | otherwise = do
      print n
      countdownPrint (n - 1)

------------------------------------------------------------------------------

-- Ex 7: Read n numbers, print running sum after each, return total sum
isums :: Int -> IO Int
isums n = isums' n 0
  where
    isums' 0 acc = return acc
    isums' k acc = do
      num <- readLn
      let sumSoFar = acc + num
      print sumSoFar
      isums' (k - 1) sumSoFar

------------------------------------------------------------------------------

-- Ex 8: whenM with IO Bool
whenM :: IO Bool -> IO () -> IO ()
whenM cond op = do
  result <- cond
  when result op

------------------------------------------------------------------------------

-- Ex 9: while loop using IO Bool condition
while :: IO Bool -> IO () -> IO ()
while cond op = do
  result <- cond
  when result $ do
    op
    while cond op

------------------------------------------------------------------------------

-- Ex 10: Print string, run op, print string again, return result
debug :: String -> IO a -> IO a
debug s op = do
  putStrLn s
  result <- op
  putStrLn s
  return result
