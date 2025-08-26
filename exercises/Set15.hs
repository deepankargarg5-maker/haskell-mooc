module Set15 where

import Mooc.Todo
import Examples.Validation

import Control.Applicative
import Data.Char
import Text.Read (readMaybe)

-- Ex 1
sumTwoMaybes :: Maybe Int -> Maybe Int -> Maybe Int
sumTwoMaybes = liftA2 (+)

-- Ex 2
statements :: [String] -> [String] -> [String]
statements xs ys = liftA2 (\x y -> [x ++ " is " ++ y, x ++ " is not " ++ y]) xs ys >>= id

-- Ex 3
calculator :: String -> String -> Maybe Int
calculator op n = liftA2 f (parseOp op) (readMaybe n)
  where
    parseOp "negate" = Just negate
    parseOp "double" = Just (*2)
    parseOp _        = Nothing
    f = ($)

-- Ex 4
validateDiv :: Int -> Int -> Validation Int
validateDiv x y = check (y /= 0) "Division by zero!" *> pure (x `div` y)

-- Ex 5
data Address = Address String String String
  deriving (Show,Eq)

validateAddress :: String -> String -> String -> Validation Address
validateAddress s n p = Address <$> valStreet s <*> valNum n <*> valPost p
  where
    valStreet str = check (length str <= 20) "Invalid street name" *> pure str
    valNum num = check (all isDigit num) "Invalid street number" *> pure num
    valPost code = check (length code == 5 && all isDigit code) "Invalid postcode" *> pure code

-- Ex 6
data Person = Person String Int Bool
  deriving (Show, Eq)

twoPersons :: Applicative f =>
  f String -> f Int -> f Bool -> f String -> f Int -> f Bool
  -> f [Person]
twoPersons n1 a1 e1 n2 a2 e2 =
  liftA2 (\p1 p2 -> [p1, p2]) (Person <$> n1 <*> a1 <*> e1)
                                (Person <$> n2 <*> a2 <*> e2)

-- Ex 7
boolOrInt :: String -> Validation (Either Bool Int)
boolOrInt s = (maybeToValidation "Not a Bool" (Left <$> readMaybe s))
          <|> (maybeToValidation "Not an Int" (Right <$> readMaybe s))

-- Helper for Validation
maybeToValidation :: String -> Maybe a -> Validation a
maybeToValidation msg = maybe (invalid msg) pure

-- Ex 8
normalizePhone :: String -> Validation String
normalizePhone input = 
  let noSpaces = filter (/= ' ') input
      tooLong = check (length noSpaces <= 10) "Too long"
      charChecks = traverse checkChar noSpaces
  in (const id <$> tooLong <*> charChecks)

  where
    checkChar c = check (isDigit c) ("Invalid character: " ++ [c]) *> pure c

-- Ex 9
data Arg = Number Int | Variable Char
  deriving (Show, Eq)

data Expression = Plus Arg Arg | Minus Arg Arg
  deriving (Show, Eq)

parseExpression :: String -> Validation Expression
parseExpression s = case words s of
  [a1, op, a2] -> makeExpr <$> parseArg a1 <*> parseOp op <*> parseArg a2
  _            -> invalid ("Invalid expression: " ++ s)
  where
    parseArg w = (Number <$> maybeToValidation ("Invalid number: " ++ w) (readMaybe w))
             <|> (case w of
                    [c] | isAlpha c -> pure (Variable c)
                    _              -> invalid ("Invalid variable: " ++ w))

    parseOp "+" = pure Plus
    parseOp "-" = pure Minus
    parseOp o   = invalid ("Unknown operator: " ++ o)

    makeExpr a Plus b  = Plus a b
    makeExpr a Minus b = Minus a b

-- Ex 10
data Priced a = Priced Int a
  deriving (Show, Eq)

instance Functor Priced where
  fmap f (Priced p x) = Priced p (f x)

instance Applicative Priced where
  pure x = Priced 0 x
  liftA2 f (Priced p1 x1) (Priced p2 x2) = Priced (p1 + p2) (f x1 x2)

-- Ex 11
class MyApplicative f where
  myPure :: a -> f a
  myLiftA2 :: (a -> b -> c) -> f a -> f b -> f c

instance MyApplicative Maybe where
  myPure = pure
  myLiftA2 = liftA2

instance MyApplicative [] where
  myPure = pure
  myLiftA2 = liftA2

(<#>) :: MyApplicative f => f (a -> b) -> f a -> f b
f <#> x = myLiftA2 ($) f x

-- Ex 12
myFmap :: MyApplicative f => (a -> b) -> f a -> f b
myFmap f x = myLiftA2 (\_ y -> f y) (myPure ()) x

-- Ex 13
tryAll :: Alternative f => (a -> f b) -> [a] -> f b
tryAll f = foldr (<|>) empty . map f

-- Ex 14
newtype Both f g a = Both (f (g a))
  deriving Show

instance (Functor f, Functor g) => Functor (Both f g) where
  fmap h (Both x) = Both (fmap (fmap h) x)

-- Ex 15
instance (Applicative f, Applicative g) => Applicative (Both f g) where
  pure x = Both (pure (pure x))
  liftA2 f (Both x) (Both y) = Both (liftA2 (liftA2 f) x y)
