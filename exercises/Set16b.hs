module Set16b (
  pounds,
  composeRates,
  Name, toFirst, toLast, fromName,
  capitalize, toFull,
  Render(..)
) where

import Mooc.Todo
import Examples.Phantom
import Data.Char (toUpper)

------------------------------------------------------------------------------
-- Ex 1

data GBP

pounds :: Money GBP
pounds = Money 3

------------------------------------------------------------------------------
-- Ex 2

composeRates :: Rate b c -> Rate a b -> Rate a c
composeRates (Rate r1) (Rate r2) = Rate (r1 * r2)

-- Example for testing
usdToChf :: Rate USD CHF
usdToChf = Rate 1.11

------------------------------------------------------------------------------
-- Ex 3

data First
data Last
data Full

newtype Name a = Name String

fromName :: Name a -> String
fromName (Name s) = s

toFirst :: String -> Name First
toFirst s = Name s

toLast :: String -> Name Last
toLast s = Name s

------------------------------------------------------------------------------
-- Ex 4

capitalize :: Name a -> Name a
capitalize (Name "")     = Name ""
capitalize (Name (c:cs)) = Name (toUpper c : cs)

toFull :: Name First -> Name Last -> Name Full
toFull (Name f) (Name l) = Name (f ++ " " ++ l)

------------------------------------------------------------------------------
-- Ex 5

class Render currency where
  render :: Money currency -> String

instance Render EUR where
  render (Money x) = show x ++ "e"

instance Render USD where
  render (Money x) = "$" ++ show x

instance Render CHF where
  render (Money x) = show x ++ "chf"
