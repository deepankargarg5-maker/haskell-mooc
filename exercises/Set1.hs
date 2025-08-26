module Set1 where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: define variables one and two. They should have type Int and
-- values 1 and 2, respectively.

one :: Int
one = 1

two :: Int
two = 2

------------------------------------------------------------------------------
-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.

double :: Integer -> Integer
double x = 2 * x

------------------------------------------------------------------------------
-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.

quadruple :: Integer -> Integer
quadruple x = double (double x)

------------------------------------------------------------------------------
-- Ex 4: define the function distance. It should take four arguments of
-- type Double: x1, y1, x2, and y2 and return the (euclidean) distance

distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

------------------------------------------------------------------------------
-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.

eeny :: Integer -> String
eeny n = if even n then "eeny" else "meeny"

------------------------------------------------------------------------------
-- Ex 6: Modify checkPassword to accept both "swordfish" and "mellon"

checkPassword :: String -> String
checkPassword password
  | password == "swordfish" = "You're in."
  | password == "mellon"    = "You're in."
  | otherwise               = "ACCESS DENIED!"

------------------------------------------------------------------------------
-- Ex 7: postal service pricing

postagePrice :: Int -> Int
postagePrice w
  | w <= 500    = 250
  | w <= 5000   = 300 + w
  | otherwise   = 6000

------------------------------------------------------------------------------
-- Ex 8: define isZero using pattern matching

isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

------------------------------------------------------------------------------
-- Ex 9: implement sumTo using recursion

sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

------------------------------------------------------------------------------
-- Ex 10: power n k using recursion

power :: Integer -> Integer -> Integer
power _ 0 = 1
power n k = n * power n (k - 1)

------------------------------------------------------------------------------
-- Ex 11: ilog3

ilog3 :: Integer -> Integer
ilog3 n
  | n <= 0    = 0
  | otherwise = 1 + ilog3 (n `div` 3)
