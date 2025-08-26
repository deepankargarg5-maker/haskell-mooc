module Set16b where

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

-- Example rate for testing
usdToChf :: Rate USD CHF
usdToChf = Rate 1.11

------------------------------------------------------------------------------
-- Ex 3

data First
