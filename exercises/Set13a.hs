{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Set13a where
import Mooc.Todo
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map as Map
import Examples.Bank

------------------------------------------------------------------------------
-- Ex 1

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> \_ = Nothing
Just x  ?> f = f x

readNames :: String -> Maybe (String,String)
readNames s =
  split s
  ?>
  checkNumber
  ?>
  checkCapitals

-- Split input into two words
split :: String -> Maybe (String,String)
split s =
  case words s of
    [a, b] -> Just (a, b)
    _      -> Nothing

-- Ensure neither string contains a digit
checkNumber :: (String, String) -> Maybe (String, String)
checkNumber (a, b)
  | any isDigit a || any isDigit b = Nothing
  | otherwise = Just (a, b)

-- Ensure both start with uppercase letter
checkCapitals :: (String, String) -> Maybe (String, String)
checkCapitals (f, s)
  | not (null f) && not (null s) && isUpper (head f) && isUpper (head s) = Just (f, s)
  | otherwise = Nothing

------------------------------------------------------------------------------
-- Ex 2

winner :: [(String,Int)] -> String -> String -> Maybe String
winner scores player1 player2 = do
  s1 <- lookup player1 scores
  s2 <- lookup player2 scores
  return $ if s1 >= s2 then player1 else player2

------------------------------------------------------------------------------
-- Ex 3

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i)

selectSum :: Num a => [a] -> [Int] -> Maybe a
selectSum xs is = fmap sum $ mapM (safeIndex xs) is

------------------------------------------------------------------------------
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
countAndLog pred xs = Logger logs (length logs)
  where logs = map show (filter pred xs)

------------------------------------------------------------------------------
-- Ex 5

balance :: String -> BankOp Int
balance accountName = BankOp $ \(Bank m) -> (Map.findWithDefault 0 accountName m, Bank m)

------------------------------------------------------------------------------
-- Ex 6

rob :: String -> String -> BankOp ()
rob from to =
  balance from +> \amt ->
    withdrawOp from amt +>
      \_ -> depositOp to amt +> \_ -> returnOp ()

------------------------------------------------------------------------------
-- Ex 7

update :: State Int ()
update = do
  modify (*2)
  modify (+1)

------------------------------------------------------------------------------
-- Ex 8

paren :: Char -> State Int ()
paren c = do
  n <- get
  case (c, n) of
    (_, -1) -> return ()
    ('(', _) -> put (n+1)
    (')', 0) -> put (-1)
    (')', _) -> put (n-1)
    _        -> return ()

parensMatch :: String -> Bool
parensMatch s = count == 0
  where (_,count) = runState (mapM_ paren s) 0

------------------------------------------------------------------------------
-- Ex 9

count :: Eq a => a -> State [(a,Int)] ()
count x = do
  occs <- get
  let (here, rest) = partition ((==x) . fst) occs
  case here of
    [(a,n)] -> put $ (a,n+1):rest
    []      -> put $ (x,1):occs
    _       -> error "Should not happen"

------------------------------------------------------------------------------
-- Ex 10

occurrences :: (Eq a) => [a] -> State [(a,Int)] Int
occurrences xs = do
  mapM_ count xs
  st <- get
  return (length st)
