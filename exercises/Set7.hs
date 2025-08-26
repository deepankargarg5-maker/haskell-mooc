module Set7 where

import Mooc.Todo
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Data.Semigroup

------------------------------------------------------------------------------
-- Ex 1: velocity and travel functions

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
-- Ex 2: Set datatype and operations

data Set a = Set [a]
  deriving (Show,Eq)

emptySet :: Set a
emptySet = Set []

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

add :: (Ord a, Eq a) => a -> Set a -> Set a
add x (Set xs)
  | x `elem` xs = Set xs
  | otherwise = Set (insert x xs)

------------------------------------------------------------------------------
-- Ex 3: Cake baking state machine

data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq,Show)

-- Define more states to track the process
data State
  = Start
  | EggsAdded
  | FlourAdded
  | SugarAdded
  | FlourSugarAdded  -- both flour and sugar added (order doesn't matter)
  | Mixed
  | Finished
  | Error
  deriving (Eq, Show)

step :: State -> Event -> State
step Error _ = Error
step Finished _ = Finished

-- From Start, only AddEggs is valid
step Start AddEggs = EggsAdded
step Start _ = Error

-- After eggs, flour or sugar can be added in any order
step EggsAdded AddFlour = FlourAdded
step EggsAdded AddSugar = SugarAdded
step EggsAdded _ = Error

-- After flour added, sugar must be added
step FlourAdded AddSugar = FlourSugarAdded
step FlourAdded _ = Error

-- After sugar added, flour must be added
step SugarAdded AddFlour = FlourSugarAdded
step SugarAdded _ = Error

-- After both flour and sugar added, only Mix is valid
step FlourSugarAdded Mix = Mixed
step FlourSugarAdded _ = Error

-- After mixing, only Bake is valid
step Mixed Bake = Finished
step Mixed _ = Error

-- For all other states, any event is an error (should not happen)
step _ _ = Error

------------------------------------------------------------------------------
-- Ex 4: average for NonEmpty list

average :: Fractional a => NonEmpty a -> a
average xs = sum xs / fromIntegral (length xs)

------------------------------------------------------------------------------
-- Ex 5: reverse a NonEmpty list

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (x :| xs) =
  case reverse (x:xs) of
    (y:ys) -> y :| ys
    [] -> error "Impossible: NonEmpty list is never empty"

------------------------------------------------------------------------------
-- Ex 6: Semigroup instances for Distance, Time and Velocity (addition)

instance Semigroup Distance where
  (Distance d1) <> (Distance d2) = Distance (d1 + d2)

instance Semigroup Time where
  (Time t1) <> (Time t2) = Time (t1 + t2)

instance Semigroup Velocity where
  (Velocity v1) <> (Velocity v2) = Velocity (v1 + v2)

------------------------------------------------------------------------------
-- Ex 7: Monoid instance for Set

instance (Ord a) => Semigroup (Set a) where
  (Set xs) <> (Set ys) = Set (union xs ys)

instance (Ord a) => Monoid (Set a) where
  mempty = emptySet

------------------------------------------------------------------------------
-- Ex 8: Operation1 and Operation2 with multiplication

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
-- Ex 9: passwordAllowed implementation

data PasswordRequirement =
  MinimumLength Int
  | ContainsSome String
  | DoesNotContain String
  | And PasswordRequirement PasswordRequirement
  | Or PasswordRequirement PasswordRequirement
  deriving Show

passwordAllowed :: String -> PasswordRequirement -> Bool
passwordAllowed pw (MinimumLength n) = length pw >= n
passwordAllowed pw (ContainsSome chars) = any (`elem` chars) pw
passwordAllowed pw (DoesNotContain chars) = all (`notElem` chars) pw
passwordAllowed pw (And r1 r2) = passwordAllowed pw r1 && passwordAllowed pw r2
passwordAllowed pw (Or r1 r2) = passwordAllowed pw r1 || passwordAllowed pw r2

------------------------------------------------------------------------------
-- Ex 10: Arithmetic DSL

data Arithmetic
  = Lit Integer
  | Op String Arithmetic Arithmetic
  deriving Show

literal :: Integer -> Arithmetic
literal = Lit

operation :: String -> Arithmetic -> Arithmetic -> Arithmetic
operation = Op

evaluate :: Arithmetic -> Integer
evaluate (Lit n) = n
evaluate (Op op l r) =
  case op of
    "+" -> evaluate l + evaluate r
    "*" -> evaluate l * evaluate r
    _ -> error "Unknown operation"

render :: Arithmetic -> String
render (Lit n) = show n
render (Op op l r) = "(" ++ render l ++ op ++ render r ++ ")"
