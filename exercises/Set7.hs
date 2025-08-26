module Set7 where

import Mooc.Todo
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Data.Semigroup

------------------------------------------------------------------------------
-- Ex 1: Time, Distance, Velocity calculations

data Distance = Distance Double
  deriving (Show,Eq)

data Time = Time Double
  deriving (Show,Eq)

data Velocity = Velocity Double
  deriving (Show,Eq)

velocity :: Distance -> Time -> Velocity
velocity (Distance d) (Time t) = Velocity (d / t)

travel :: Velocity -> Time -> Distance
travel (Velocity v) (Time t) = Distance (v * t)

------------------------------------------------------------------------------
-- Ex 2: Simple Set datatype with unique ordered elements

data Set a = Set [a]
  deriving (Show,Eq)

emptySet :: Set a
emptySet = Set []

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

add :: Ord a => a -> Set a -> Set a
add x (Set xs) = Set $ insertUnique x xs
  where
    insertUnique y [] = [y]
    insertUnique y l@(z:zs)
      | y == z = l
      | y < z  = y : l
      | otherwise = z : insertUnique y zs

------------------------------------------------------------------------------
-- Ex 3: Cake baking state machine

data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq,Show)

data State
  = Start
  | EggsAdded
  | FlourAdded
  | SugarAdded
  | FlourSugarAdded
  | Mixed
  | Finished
  | Error
  deriving (Eq, Show)

step :: State -> Event -> State
step Error _ = Error
step Finished _ = Finished
step Start AddEggs = EggsAdded
step EggsAdded AddFlour = FlourAdded
step EggsAdded AddSugar = SugarAdded
step FlourAdded AddSugar = FlourSugarAdded
step SugarAdded AddFlour = FlourSugarAdded
step FlourSugarAdded Mix = Mixed
step Mixed Bake = Finished
step _ _ = Error

bake :: [Event] -> State
bake events = go Start events
  where
    go state [] = state
    go state (e:es) = go (step state e) es

------------------------------------------------------------------------------
-- Ex 4: average for NonEmpty lists

average :: Fractional a => NonEmpty a -> a
average (x :| xs) = (x + sum xs) / fromIntegral (1 + length xs)

------------------------------------------------------------------------------
-- Ex 5: reverse a NonEmpty list

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (x :| xs) =
  case reverse (x:xs) of
    [] -> error "Impossible: NonEmpty reverse is empty"
    (y:ys) -> y :| ys

------------------------------------------------------------------------------
-- Ex 6: Semigroup instances for Distance, Time, Velocity

instance Semigroup Distance where
  (Distance d1) <> (Distance d2) = Distance (d1 + d2)

instance Semigroup Time where
  (Time t1) <> (Time t2) = Time (t1 + t2)

instance Semigroup Velocity where
  (Velocity v1) <> (Velocity v2) = Velocity (v1 + v2)

------------------------------------------------------------------------------
-- Ex 7: Monoid instance for Set

instance (Ord a) => Semigroup (Set a) where
  (Set xs) <> (Set ys) = Set (merge xs ys)
    where
      merge [] ys = ys
      merge xs [] = xs
      merge l1@(x:xs) l2@(y:ys)
        | x < y     = x : merge xs l2
        | x > y     = y : merge l1 ys
        | otherwise = x : merge xs ys

instance (Ord a) => Monoid (Set a) where
  mempty = Set []

------------------------------------------------------------------------------
-- Ex 8: Operation1 and Operation2 with multiply added

data Operation1 = Add1 Int Int
                | Subtract1 Int Int
                | Multiply1 Int Int
  deriving Show

compute1 :: Operation1 -> Int
compute1 (Add1 i j) = i + j
compute1 (Subtract1 i j) = i - j
compute1 (Multiply1 i j) = i * j

show1 :: Operation1 -> String
show1 (Add1 i j) = show i ++ "+" ++ show j
show1 (Subtract1 i j) = show i ++ "-" ++ show j
show1 (Multiply1 i j) = show i ++ "*" ++ show j

data Add2 = Add2 Int Int
  deriving Show
data Subtract2 = Subtract2 Int Int
  deriving Show
data Multiply2 = Multiply2 Int Int
  deriving Show

class Operation2 op where
  compute2 :: op -> Int
  show2 :: op -> String

instance Operation2 Add2 where
  compute2 (Add2 i j) = i + j
  show2 (Add2 i j) = show i ++ "+" ++ show j

instance Operation2 Subtract2 where
  compute2 (Subtract2 i j) = i - j
  show2 (Subtract2 i j) = show i ++ "-" ++ show j

instance Operation2 Multiply2 where
  compute2 (Multiply2 i j) = i * j
  show2 (Multiply2 i j) = show i ++ "*" ++ show j

------------------------------------------------------------------------------
-- Ex 9: Password validation

data PasswordRequirement =
  MinimumLength Int
  | ContainsSome String    -- contains at least one of given characters
  | DoesNotContain String  -- does not contain any of the given characters
  | And PasswordRequirement PasswordRequirement -- and'ing two requirements
  | Or PasswordRequirement PasswordRequirement  -- or'ing
  deriving Show

passwordAllowed :: String -> PasswordRequirement -> Bool
passwordAllowed password (MinimumLength n) = length password >= n
passwordAllowed password (ContainsSome chars) = any (`elem` chars) password
passwordAllowed password (DoesNotContain chars) = all (`notElem` chars) password
passwordAllowed password (And req1 req2) = passwordAllowed password req1 && passwordAllowed password req2
passwordAllowed password (Or req1 req2) = passwordAllowed password req1 || passwordAllowed password req2

------------------------------------------------------------------------------
-- Ex 10: Arithmetic expressions with addition and multiplication

data Arithmetic
  = Literal Integer
  | Operation String Arithmetic Arithmetic
  deriving Show

literal :: Integer -> Arithmetic
literal = Literal

operation :: String -> Arithmetic -> Arithmetic -> Arithmetic
operation = Operation

evaluate :: Arithmetic -> Integer
evaluate (Literal x) = x
evaluate (Operation "+" a b) = evaluate a + evaluate b
evaluate (Operation "*" a b) = evaluate a * evaluate b
evaluate _ = error "Unsupported operation"

render :: Arithmetic -> String
render (Literal x) = show x
render (Operation op a b) = "(" ++ render a ++ op ++ render b ++ ")"
