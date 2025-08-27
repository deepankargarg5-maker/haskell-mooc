{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Set15 where

import Mooc.Todo
import Examples.Validation
import Control.Applicative
import Data.Char
import Text.Read (readMaybe)

data Address = Address String String String
  deriving (Show, Eq)

-- helper joiner for multiple errors
joinErrors :: [String] -> String
joinErrors [] = ""
joinErrors [e] = e
joinErrors (e:es) = e ++ "; " ++ joinErrors es

-- Ex 1
sumTwoMaybes :: Maybe Int -> Maybe Int -> Maybe Int
sumTwoMaybes = liftA2 (+)

-- Ex 2
statements :: [String] -> [String] -> [String]
statements xs ys =
  liftA2 (\x y -> x ++ " is " ++ y) xs ys ++ liftA2 (\x y -> x ++ " is not " ++ y) xs ys

-- Ex 3
calculator :: String -> String -> Maybe Int
calculator op n = do
  v <- readMaybe n
  f <- case op of
    "negate" -> Just negate
    "double" -> Just (*2)
    _ -> Nothing
  return (f v)

-- Ex 4
validateDiv :: Int -> Int -> Validation Int
validateDiv _ 0 = invalid "Division by zero!"
validateDiv x y = pure (x `div` y)

-- Ex 5
validateAddress :: String -> String -> String -> Validation Address
validateAddress streetName streetNumber postCode =
  Address
    <$> checkStreet streetName
    <*> checkNumber streetNumber
    <*> checkPostcode postCode
  where
    checkStreet s =
      if length s <= 20 then pure s else invalid "Invalid street name"
    checkNumber s =
      if all isDigit s then pure s else invalid "Invalid street number"
    checkPostcode s =
      if length s == 5 && all isDigit s then pure s else invalid "Invalid postcode"

-- Ex 6
data Person = Person String Int Bool
  deriving (Show, Eq)

twoPersons :: Applicative f =>
  f String -> f Int -> f Bool -> f String -> f Int -> f Bool -> f [Person]
twoPersons n1 a1 e1 n2 a2 e2 =
  (:) <$> (Person <$> n1 <*> a1 <*> e1)
      <*> ((:[]) <$> (Person <$> n2 <*> a2 <*> e2))

-- Ex 7
boolOrInt :: String -> Validation (Either Bool Int)
boolOrInt s =
  let b = case map toLower s of
            "true"  -> pure (Left True)
            "false" -> pure (Left False)
            _       -> invalid "Not a Bool"
      i = case readMaybe s of
            Just n  -> pure (Right n)
            Nothing -> invalid "Not an Int"
  in b <|> i

-- Ex 8
normalizePhone :: String -> Validation String
normalizePhone s =
  let stripped = filter (/= ' ') s
      tooLong = if length stripped > 10 then invalid "Too long" else pure ()
      checkChars [] errs acc =
        if null errs then pure (reverse acc)
        else invalid (joinErrors (map (\c -> "Invalid character: " ++ [c]) errs))
      checkChars (c:cs) errs acc
        | isDigit c = checkChars cs errs (c:acc)
        | otherwise = checkChars cs (c:errs) acc
  in (\_ res -> res) <$> tooLong <*> checkChars stripped [] []

-- Ex 9
data Arg = Number Int | Variable Char
  deriving (Show, Eq)

data Expression = Plus Arg Arg | Minus Arg Arg
  deriving (Show, Eq)

parseExpression :: String -> Validation Expression
parseExpression s =
  case words s of
    [arg1, op, arg2] ->
      let parseOp = case op of
            "+" -> pure Plus
            "-" -> pure Minus
            _   -> invalid ("Unknown operator: " ++ op)
          parseArg str =
            let isVar = length str == 1 && isAlpha (head str)
                isNum = all isDigit str || ((not . null) str && head str == '-' && all isDigit (tail str))
            in case (isVar, isNum, readMaybe str :: Maybe Int) of
                 (True, False, _) -> pure (Variable (head str))
                 (False, True, Just n) -> pure (Number n)
                 (False, True, Nothing) -> invalid (joinErrors ["Invalid number: " ++ str, "Invalid variable: " ++ str])
                 (False, False, _) -> invalid (joinErrors ["Invalid number: " ++ str, "Invalid variable: " ++ str])
                 (True, True, _) -> pure (Variable (head str))
          a1 = parseArg arg1
          a2 = parseArg arg2
      in parseOp <*> a1 <*> a2
    _ -> invalid ("Invalid expression: " ++ s)
