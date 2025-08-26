{-# LANGUAGE NoImplicitPrelude #-}

module Set10b where

import Mooc.VeryLimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------

-- Ex 1: (|||) that forces the *right* argument only

(|||) :: Bool -> Bool -> Bool
False ||| y = y
True  ||| _ = True

------------------------------------------------------------------------------

-- Ex 2: Count length of list and force all elements

boolLength :: [Bool] -> Int
boolLength [] = 0
boolLength (x:xs) =
  case x of
    True  -> 1 + boolLength xs
    False -> 1 + boolLength xs

------------------------------------------------------------------------------

-- Ex 3: Force predicate result but return value

validate :: (a -> Bool) -> a -> a
validate predicate value =
  case predicate value of
    True  -> value
    False -> value

------------------------------------------------------------------------------

-- Ex 4: Simulate seq using pattern matching

class MySeq a where
  myseq :: a -> b -> b

-- Force evaluation of Bool
instance MySeq Bool where
  myseq x y = case x of
    True  -> y
    False -> y

-- Force evaluation of Int
instance MySeq Int where
  myseq x y = case x of
    0 -> y
    _ -> y

-- Lists are lazy by default; only top constructor is matched
instance MySeq [a] where
  myseq x y = case x of
    []     -> y
    (_:_)  -> y
