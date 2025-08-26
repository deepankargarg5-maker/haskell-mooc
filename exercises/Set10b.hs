{-# LANGUAGE NoImplicitPrelude #-}

module Set10b where

import Mooc.VeryLimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------

-- Ex 1: ||| forces its right argument instead of left.
(|||) :: Bool -> Bool -> Bool
False ||| y = y
True  ||| _ = True

------------------------------------------------------------------------------

-- Ex 2: boolLength forces all elements in the list
boolLength :: [Bool] -> Int
boolLength [] = 0
boolLength (x:xs) = if x then 1 + boolLength xs else 1 + boolLength xs

------------------------------------------------------------------------------

-- Ex 3: force the predicate even though we don’t use its result
validate :: (a -> Bool) -> a -> a
validate f x = case f x of
                 True -> x
                 False -> x

------------------------------------------------------------------------------

-- Ex 4: custom seq (called myseq)

class MySeq a where
  myseq :: a -> b -> b

instance MySeq Bool where
  myseq True  y = y
  myseq False y = y
  myseq x     y = case x of
                    True -> y
                    False -> y

instance MySeq Int where
  myseq 0 y = y
  myseq 1 y = y
  myseq x y = case x of
                0 -> y
                _ -> y

instance MySeq [a] where
  myseq [] y = y
  myseq (_:_) y = y
  myseq x y = case x of
                [] -> y
                (_:_) -> y
