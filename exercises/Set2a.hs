module Set2a where

import Data.List

-- Ex 1
years :: [Int]
years = [1982, 2004, 2020]

-- Ex 2
takeFinal :: Int -> [a] -> [a]
takeFinal n xs
  | n >= length xs = xs
  | otherwise      = drop (length xs - n) xs

-- Ex 3
updateAt :: Int -> a -> [a] -> [a]
updateAt i x xs = take i xs ++ [x] ++ drop (i+1) xs

-- Ex 4
substring :: Int -> Int -> String -> String
substring i j s = take (j - i) (drop i s)

-- Ex 5
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

-- Ex 6
palindromify :: String -> String
palindromify s
  | isPalindrome s = s
  | null s        = s
  | otherwise     = palindromify (init (tail s))

-- Ex 7
safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- Ex 8
greet :: String -> Maybe String -> String
greet first Nothing = "Hello, " ++ first ++ "!"
greet first (Just last) = "Hello, " ++ first ++ " " ++ last ++ "!"

-- Ex 9
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 = Nothing
  | i >= length xs = Nothing
  | otherwise = Just (xs !! i)

-- Ex 10
eitherDiv :: Integer -> Integer -> Either String Integer
eitherDiv x 0 = Left (show x ++ "/0")
eitherDiv x y = Right (x `div` y)

-- Ex 11
addEithers :: Either String Int -> Either String Int -> Either String Int
addEithers (Right a) (Right b) = Right (a + b)
addEithers (Left e) _ = Left e
addEithers _ (Left e) = Left e
