module Set16b where

import Mooc.Todo
import Examples.Phantom

import Data.Char (toUpper)

-- Exercise 1

data GBP

pounds :: Money GBP
pounds = Money 3.0

-- Exercise 2

composeRates :: Rate b c -> Rate a b -> Rate a c
composeRates (Rate r1) (Rate r2) = Rate (r1 * r2)

-- Exercise 3

data First
data Last
data Full

newtype Name a = Name String
  deriving Show

fromName :: Name a -> String
fromName (Name str) = str

toFirst :: String -> Name First
toFirst = Name

toLast :: String -> Name Last
toLast = Name

-- Exercise 4

capitalize :: Name a -> Name a
capitalize (Name "") = Name ""
capitalize (Name (x:xs)) = Name (toUpper x : xs)

toFull :: Name First -> Name Last -> Name Full
toFull (Name f) (Name l) = Name (f ++ " " ++ l)

-- Exercise 5

data EUR
data USD
data CHF

instance Render EUR where
  render (Money x) = show x ++ "e"

instance Render USD where
  render (Money x) = "$" ++ show x

instance Render CHF where
  render (Money x) = show x ++ "chf"
