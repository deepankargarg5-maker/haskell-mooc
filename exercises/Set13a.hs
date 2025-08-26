module Set13a where

import Mooc.Todo
import Control.Monad
import qualified Data.Map as Map

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

instance Monad BankOp where
  return = pure
  (BankOp x) >>= f = BankOp $ \bank ->
    let (y, bank') = x bank
        (BankOp z) = f y
     in z bank'

------------------------------------------------------------------------------
-- Ex 2: Get the balance of an account

getBalance :: Account -> BankOp Int
getBalance acc = BankOp $ \bank ->
  let amount = Map.findWithDefault 0 acc bank
   in (amount, bank)

------------------------------------------------------------------------------
-- Ex 3: Deposit money into an account

deposit :: Account -> Amount -> BankOp ()
deposit acc amt = BankOp $ \bank ->
  let newBal = amt + Map.findWithDefault 0 acc bank
      newBank = Map.insert acc newBal bank
   in ((), newBank)

------------------------------------------------------------------------------
-- Ex 4: Withdraw money from an account

withdraw :: Account -> Amount -> BankOp ()
withdraw acc amt = BankOp $ \bank ->
  let oldBal = Map.findWithDefault 0 acc bank
      newBank = Map.insert acc (oldBal - amt) bank
   in ((), newBank)

------------------------------------------------------------------------------
-- Ex 5: A helper for composing BankOps

(+>) :: BankOp a -> BankOp b -> BankOp b
(+>) = (>>)

------------------------------------------------------------------------------
-- Ex 6: Define operations using do-notation

depositDo :: Account -> Amount -> BankOp ()
depositDo acc amt = do
  bal <- getBalance acc
  deposit acc amt

withdrawDo :: Account -> Amount -> BankOp ()
withdrawDo acc amt = do
  bal <- getBalance acc
  withdraw acc amt

------------------------------------------------------------------------------
-- Ex 7: Log each step

logOp :: String -> BankOp ()
logOp msg = BankOp $ \bank ->
  trace msg ((), bank)

-- Comment out or delete if you don't want trace output
trace :: String -> a -> a
trace msg x = unsafePerformIO (putStrLn msg >> return x)

import System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------------
-- Ex 8: A conditional BankOp

ifM :: BankOp Bool -> BankOp a -> BankOp a -> BankOp a
ifM cond ifTrue ifFalse = do
  b <- cond
  if b then ifTrue else ifFalse

------------------------------------------------------------------------------
-- Ex 9: A conditional transfer

-- Helper to withdraw and deposit
withdrawOp :: Account -> Int -> BankOp ()
withdrawOp = withdraw

depositOp :: Account -> Int -> BankOp ()
depositOp = deposit

transfer :: Account -> Account -> BankOp (Int -> BankOp ())
transfer from to = return $ \amount ->
  withdrawOp from amount +> depositOp to amount
