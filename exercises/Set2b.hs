module Set2b where

import Data.List

-- Ex 1
binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 k
  | k > 0 = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

-- Ex 2
oddFactorial :: Integer -> Integer
oddFactorial n
  | n <= 0 = 1
  | even n = oddFactorial (n-1)
  | otherwise = n * oddFactorial (n-2)

-- Ex 3
myGcd :: Integer -> Integer -> Integer
myGcd a 0 = abs a
myGcd 0 b = abs b
myGcd a b
  | a == b = abs a
  | a > b = myGcd (a - b) b
  | otherwise = myGcd a (b - a)

-- Ex 4
leftpad :: String -> Int -> String
leftpad s n
  | length s >= n = s
  | otherwise = replicate (n - length s) ' ' ++ s

-- Ex 5
countdown :: Integer -> String
countdown n = "Ready! " ++ countdownHelper n ++ "Liftoff!"
  where
    countdownHelper 0 = ""
    countdownHelper x = show x ++ "... " ++ countdownHelper (x-1)

-- Ex 6
smallestDivisor :: Integer -> Integer
smallestDivisor n = findDivisor 2
  where
    findDivisor d
      | d * d > n = n
      | n `mod` d == 0 = d
      | otherwise = findDivisor (d+1)

-- Ex 7
isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | otherwise = smallestDivisor n == n

-- Ex 8
biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost n
  | n < 2 = error "Input less than 2 is undefined"
  | isPrime n = n
  | otherwise = biggestPrimeAtMost (n-1)
