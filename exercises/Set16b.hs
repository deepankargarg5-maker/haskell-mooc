{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Set16b where
import Mooc.Todo
import Examples.Phantom
import Data.Char (toUpper)

-- Ex 1
-- Define phantom type GBP
data GBP

pounds :: Money GBP
pounds = Money 3

-- Ex 2
-- Compose rates with phantom currency tracking
composeRates ::
  Rate a b -> Rate b c -> Rate a c
composeRates (Rate r1) (Rate r2) = Rate (r1 * r2)

-- For testing
usdToChf :: Rate USD CHF
usdToChf = Rate 1.11

-- Ex 3
-- Phantom types for names
data First
data Last
data Full

newtype Name a = Name String

fromName :: Name a -> String
fromName (Name s) = s

toFirst :: String -> Name First
toFirst = Name

toLast :: String -> Name Last
toLast = Name

-- Ex 4
capitalize :: Name a -> Name a
capitalize (Name "") = Name ""
capitalize (Name (c:cs)) = Name (toUpper c : cs)

toFull :: Name First -> Name Last -> Name Full
toFull (Name f) (Name l) = Name (f ++ " " ++ l)

-- Ex 5
class Render currency where
  render :: Money currency -> String

instance Render EUR where
  render (Money amount) = show amount ++ "e"

instance Render USD where
  render (Money amount) = "$" ++ show amount

instance Render CHF where
  render (Money amount) = show amount ++ "chf"
