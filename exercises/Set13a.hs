{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Set13a where

import Mooc.Todo

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map as Map

import Examples.Bank

-- | Custom operator for chaining Maybes
(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing
Just x  ?> f = f x

-- DO NOT touch this definition!
readNames :: String -> Maybe (String,String)
readNames s =
  split s
  ?>
  checkNumber
  ?>
  checkCapitals

-- Ex 1
split :: String -> Maybe (String,String)
split s = case break (== ' ') s of
  (_, "") -> Nothing
  (a, _:b) -> Just (a, b)

checkNumber :: (String, String) -> Maybe (String, String)
checkNumber (a,b)
  | all (not . isDigit) a && all (not . isDigit) b = Just (a,b)
  | otherwise = Nothing

checkCapitals :: (String, String) -> Maybe (String, String)
checkCapitals (a@(c:_), b@(d:_))
  | isUpper c && isUpper d = Just (a,b)
  | otherwise = Nothing
checkCapitals _ = Nothing

-- Ex 2
winner :: [(String,Int)] -> String -> String -> Maybe String
winner scores player1 player2 = do
  s1 <- lookup player1 scores
  s2 <- lookup player2 scores
  return $ if s1 >= s2 then player1 else player2

-- Ex 3
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i)

selectSum :: Num a => [a] -> [Int] -> Maybe a
selectSum xs is = do
  values <- mapM (safeIndex xs) is
  return (sum values)

-- Ex 4
data Logger a = Logger [String] a
  deriving (Show, Eq)

msg :: String -> Logger ()
msg s = Logger [s] ()

instance Functor Logger where
  fmap f (Logger l a) = Logger l (f a)

instance Monad Logger where
  return x = Logger [] x
  Logger la a >>= f = Logger (la++lb) b
    where Logger lb b = f a

instance Applicative Logger where
  pure = return
  (<*>) = ap

countAndLog :: Show a => (a -> Bool) -> [a] -> Logger Int
countAndLog p [] = return 0
countAndLog p (x:xs)
  | p x       = do msg (show x)
                   r <- countAndLog p xs
                   return (1 + r)
  | otherwise = countAndLog p xs

-- Ex 5
balance :: String -> BankOp Int
balance acc = BankOp $ \bank ->
  let Bank m = bank
      bal = Map.findWithDefault 0 acc m
  in (bal, bank)

-- Ex 6
rob :: String -> String -> BankOp ()
rob from to =
  balance from +> \amount ->
  withdrawOp from amount +>
  depositOp to amount

-- Ex 7
update :: State Int ()
update = do
  n <- get
  put (n*2 + 1)

-- Ex 8
paren :: Char -> State Int ()
paren '(' = do
  s <- get
  if s == -1 then return () else put (s + 1)
paren ')' = do
  s <- get
  if s == 0 || s == -1 then put (-1) else put (s - 1)
paren _ = return ()

parensMatch :: String -> Bool
parensMatch s = count == 0
  where (_,count) = runState (mapM_ paren s) 0

-- Ex 9
count :: Eq a => a -> State [(a,Int)] ()
count x = do
  s <- get
  case lookup x s of
    Just n  -> put ((x,n+1) : filter ((/= x) . fst) s)
    Nothing -> put ((x,1):s)

-- Ex 10
occurrences :: Eq a => [a] -> State [(a,Int)] Int
occurrences xs = do
  mapM_ count xs
  s <- get
  return (length s)
