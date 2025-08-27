{-# LANGUAGE NoImplicitPrelude #-}
module Set10b where

import Mooc.VeryLimitedPrelude
import Mooc.Todo

-- Ex 1: Define ||| to force right argument only
(|||) :: Bool -> Bool -> Bool
x ||| y = case y of      -- force right argument y
  True -> True
  False -> x

-- Ex 2: Length of list of Bools forcing all elements
boolLength :: [Bool] -> Int
boolLength [] = 0
boolLength (b:bs) = case b of  -- force head b
  True -> 1 + boolLength bs
  False -> 1 + boolLength bs

-- Ex 3: Validate predicate result but return original value
validate :: (a -> Bool) -> a -> a
validate pred v = case pred v of
  True -> v
  False -> v

-- Ex 4: MySeq class with instances
class MySeq a where
  myseq :: a -> b -> b

instance MySeq Bool where
  myseq b x = case b of
    True -> x
    False -> x

instance MySeq Int where
  myseq i x = case i + 0 of   -- Adding 0 forces evaluation
    _ -> x

instance MySeq [a] where
  myseq xs x = case xs of
    [] -> x
    (_:_) -> x
