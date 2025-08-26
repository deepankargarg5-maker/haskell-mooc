{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Set13b where

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.IORef
import Data.List

-- Ex 1
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM opBool opThen opElse = do
  b <- opBool
  if b then opThen else opElse

-- Ex 2
mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 _ [] _ = return []
mapM2 _ _ [] = return []
mapM2 f (x:xs) (y:ys) = do
  r <- f x y
  rs <- mapM2 f xs ys
  return (r:rs)

-- Ex 3
visit :: [(String, [String])] -> String -> State [String] ()
visit maze place = do
  visited <- get
  if place `elem` visited then return ()
  else do
    put (place : visited)
    let neighbors = maybe [] id (lookup place maze)
    mapM_ (visit maze) neighbors

path :: [(String, [String])] -> String -> String -> Bool
path maze start end = end `elem` visited
  where (_, visited) = runState (visit maze start) []

-- Ex 4
findSum2 :: [Int] -> [Int] -> [(Int, Int, Int)]
findSum2 ks ns = do
  i <- ks
  j <- ks
  let s = i + j
  guard (s `elem` ns)
  return (i, j, s)

-- Ex 5
allSums :: [Int] -> [Int]
allSums [] = [0]
allSums (x:xs) = do
  rest <- allSums xs
  [rest, x + rest]

-- Ex 6
f1 :: Int -> Int -> Int -> Maybe Int
f1 k acc x = let sum = acc + x in if sum > k then Nothing else Just sum

f2 :: Int -> Int -> State [Int] Int
f2 acc x = do
  seen <- get
  if x `elem` seen then return acc
  else do
    put (x : seen)
    return (acc + x)

-- Ex 7
data Result a = MkResult a | NoResult | Failure String deriving (Show, Eq)

instance Functor Result where
  fmap _ NoResult = NoResult
  fmap _ (Failure msg) = Failure msg
  fmap f (MkResult x) = MkResult (f x)

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return = MkResult
  MkResult x >>= f = f x
  NoResult >>= _ = NoResult
  Failure msg >>= _ = Failure msg

-- Ex 8
data SL a = SL (Int -> (a, Int, [String]))

runSL :: SL a -> Int -> (a, Int, [String])
runSL (SL f) state = f state

msgSL :: String -> SL ()
msgSL msg = SL (\s -> ((), s, [msg]))

getSL :: SL Int
getSL = SL (\s -> (s, s, []))

putSL :: Int -> SL ()
putSL s' = SL (\_ -> ((), s', []))

modifySL :: (Int -> Int) -> SL ()
modifySL f = SL (\s -> ((), f s, []))

instance Functor SL where
  fmap f (SL run) = SL $ \s -> let (a, s', logs) = run s in (f a, s', logs)

instance Applicative SL where
  pure = return
  (<*>) = ap

instance Monad SL where
  return x = SL (\s -> (x, s, []))
  SL run1 >>= f = SL $ \s ->
    let (a, s1, logs1) = run1 s
        SL run2 = f a
        (b, s2, logs2) = run2 s1
    in (b, s2, logs1 ++ logs2)

-- Ex 9
mkCounter :: IO (IO (), IO Int)
mkCounter = do
  ref <- newIORef 0
  let inc = modifyIORef ref (+1)
      get = readIORef ref
  return (inc, get)
