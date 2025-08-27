{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Set15 where

import Mooc.Todo
import Examples.Validation
import Control.Applicative
import Data.Char (isDigit, isAlpha, toLower)
import Text.Read (readMaybe)

-- Helper to join error strings
joinErrors :: [String] -> String
joinErrors = foldr1 (\e acc -> if null acc then e else e ++ "; " ++ acc)

-- Ex 1
sumTwoMaybes :: Maybe Int -> Maybe Int -> Maybe Int
sumTwoMaybes = liftA2 (+)

-- Ex 2
statements :: [String] -> [String] -> [String]
statements xs ys =
  liftA2 (\x y -> x ++ " is " ++ y) xs ys ++
  liftA2 (\x y -> x ++ " is not " ++ y) xs ys

-- Ex 3
calculator :: String -> String -> Maybe Int
calculator op nStr = do
  v <- readMaybe nStr
  f <- case op of
    "negate" -> Just negate
    "double" -> Just (*2)
    _        -> Nothing
  return (f v)

-- Ex 4
validateDiv :: Int -> Int -> Validation Int
validateDiv _ 0 = invalid "Division by zero!"
validateDiv x y = pure (x `div` y)

-- Ex 5
data Address = Address String String String
  deriving (Show, Eq)

validateAddress :: String -> String -> String -> Validation Address
validateAddress street sNum pCode =
  Address <$> checkStreet street <*> checkNumber sNum <*> checkPostcode pCode
  where
    checkStreet s = if length s <= 20 then pure s else invalid "Invalid street name"
    checkNumber s = if all isDigit s then pure s else invalid "Invalid street number"
    checkPostcode s = if length s == 5 && all isDigit s then pure s else invalid "Invalid postcode"

-- Ex 6
data Person = Person String Int Bool
  deriving (Show, Eq)

twoPersons ::
  Applicative f =>
  f String -> f Int -> f Bool ->
  f String -> f Int -> f Bool ->
  f [Person]
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
parseExpression s = case words s of
  [a1, op, a2] ->
    let parseOp = case op of
          "+" -> pure Plus
          "-" -> pure Minus
          _   -> invalid $ "Unknown operator: " ++ op
        parseArg str
          | all isDigit str || (not (null str) && head str == '-' && all isDigit (tail str)) =
              case readMaybe str of
                Just n  -> pure (Number n)
                Nothing -> invalid $ "Invalid number: " ++ str
          | length str == 1 && isAlpha (head str) = pure (Variable $ head str)
          | otherwise = invalid $ "Invalid argument: " ++ str
    in parseOp <*> parseArg a1 <*> parseArg a2
  _ -> invalid $ "Invalid expression: " ++ s

-- Ex 10: Priced
data Priced a = Priced Int a
  deriving (Show, Eq)

instance Functor Priced where
  fmap f (Priced cost v) = Priced cost (f v)

instance Applicative Priced where
  pure v = Priced 0 v
  Priced c1 f <*> Priced c2 v = Priced (c1 + c2) (f v)

-- Ex 11
infixl 4 <#>
(<#>) :: Applicative f => f (a -> b) -> f a -> f b
(<#>) = (<*>)

-- Ex 12
myFmap :: Functor f => (a -> b) -> f a -> f b
myFmap = fmap

-- Ex 13
tryAll :: Alternative f => (a -> f b) -> [a] -> f b
tryAll _ []     = empty
tryAll f (x:xs) = f x <|> tryAll f xs

-- Ex 14 & 15: Both with Full Applicative
data Both f g a = Both (f a) (g a)
  deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Both f g) where
  fmap h (Both x y) = Both (fmap h x) (fmap h y)

instance (Applicative f, Applicative g) => Applicative (Both f g) where
  pure v = Both (pure v) (pure v)
  Both f1 g1 <*> Both f2 g2 = Both (f1 <*> f2) (g1 <*> g2)
