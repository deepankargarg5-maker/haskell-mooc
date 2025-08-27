{-# LANGUAGE NoImplicitPrelude #-}
module Set10b where

import Mooc.VeryLimitedPrelude
import Mooc.Todo

-- Ex 1: Define ||| to behave like || but force the right argument
-- Tests fail if left argument is forced
(|||) :: Bool -> Bool -> Bool
True ||| _ = True            -- short-circuit if True on left, do not evaluate right
False ||| y = y              -- force right argument y here (due to pattern match)

-- Ex 2: Length of list of Bools forcing all elements
boolLength :: [Bool] -> Int
boolLength [] = 0
boolLength (b:bs) =
  case b of              -- forcing b by pattern matching
    True -> 1 + boolLength bs
    False -> 1 + boolLength bs

-- Ex 3: validate evaluates value and forces predicate result but discards predicate result
validate :: (a -> Bool) -> a -> a
validate pred v =
  case pred v of        -- force predicate v
    True -> v
    False -> v

-- Ex 4: MySeq class implementations
class MySeq a where
  myseq :: a -> b -> b

instance MySeq Bool where
  myseq b x =
    case b of       -- force Boolean b
      True -> x
      False -> x

instance MySeq Int where
  myseq i x =
    case i of       -- force Int i
      _ -> x

instance MySeq [a] where
  myseq xs x =
    case xs of      -- force list constructor
      [] -> x
      (_:_) -> x
