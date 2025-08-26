module Set13a where

import Mooc.Todo
import Control.Monad
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)  -- ✅ Moved here

------------------------------------------------------------------------------
-- Ex 1: The BankOp monad

type Account = String
type Amount  = Int
type Bank    = Map.Map Account Int

newtype BankOp a = BankOp { runBankOp :: Bank -> (a, Bank) }

instance Functor BankOp where
  fmap f (BankOp op) = BankOp $ \bank ->
    let (x, bank') = op bank
     in (f x, bank')

instance Applicative BankOp where
  pure x = BankOp (\bank -> (x, bank))
  (BankOp f) <*> (BankOp x) = BankOp $ \bank ->
    let (g, bank') = f bank
        (y, bank'') = x bank'
     in (g y, bank'')

instance Monad BankOp wh
