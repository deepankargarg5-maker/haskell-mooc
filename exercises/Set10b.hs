{-# LANGUAGE NoImplicitPrelude #-}
module Set10b where

import Mooc.VeryLimitedPrelude
import Mooc.Todo

-- Ex 1
(|||) :: Bool -> Bool -> Bool
x ||| y = case y of       -- force right argument only
  True -> True
  False -> x

-- Ex 2
boolLength :: [Bool] -> Int
boolLength [] = 0
boolLength (b:bs) = case b of
  True -> 1 + boolLength bs
  False -> 1 + boolLength bs

-- Ex 3
validate :: (a -> Bool) -> a -> a
validate pred v = case pred v of
  True -> v
  False -> v

-- Ex 4
class MySeq a where
  myseq :: a -> b -> b

instance MySeq Bool where
  myseq b x = case b of
    True -> x
    False -> x

instance MySeq Int where
  myseq i x = case i of
    _ -> x

instance MySeq [a] where
  myseq xs x = case xs of
    [] -> x
    (_:_) -> x
