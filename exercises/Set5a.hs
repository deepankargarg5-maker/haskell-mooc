module Set5a where

import Mooc.Todo

-- Ex 1: Define Vehicle type with four constructors, no fields
data Vehicle = Bike | Bus | Tram | Train
  deriving Show

-- Ex 2: BusTicket can be SingleTicket or MonthlyTicket with month string
data BusTicket = SingleTicket | MonthlyTicket String
  deriving Show

-- Ex 3: ShoppingEntry is given; implement totalPrice and buyOneMore

data ShoppingEntry = MkShoppingEntry String Double Int
  deriving Show

threeApples :: ShoppingEntry
threeApples = MkShoppingEntry "Apple" 0.5 3

twoBananas :: ShoppingEntry
twoBananas = MkShoppingEntry "Banana" 1.1 2

totalPrice :: ShoppingEntry -> Double
totalPrice (MkShoppingEntry _ price count) = price * fromIntegral count

buyOneMore :: ShoppingEntry -> ShoppingEntry
buyOneMore (MkShoppingEntry name price count) = MkShoppingEntry name price (count + 1)

-- Ex 4: Person datatype with age and name, with getter and setter functions

data Person = Person Int String
  deriving Show

fred :: Person
fred = Person 90 "Fred"

getName :: Person -> String
getName (Person _ name) = name

getAge :: Person -> Int
getAge (Person age _) = age

setName :: String -> Person -> Person
setName newName (Person age _) = Person age newName

setAge :: Int -> Person -> Person
setAge newAge (Person _ name) = Person newAge name

-- Ex 5: Position with two Ints, and functions to operate on it

data Position = Position Int Int
  deriving Show

origin :: Position
origin = Position 0 0

getX :: Position -> Int
getX (Position x _) = x

getY :: Position -> Int
getY (Position _ y) = y

up :: Position -> Position
up (Position x y) = Position x (y + 1)

right :: Position -> Position
right (Position x y) = Position (x + 1) y

-- Ex 6: Student datatype with study function advancing the student

data Student = Freshman | NthYear Int | Graduated
  deriving (Show,Eq)

study :: Student -> Student
study Freshman = NthYear 1
study (NthYear n)
  | n < 7 = NthYear (n + 1)
  | otherwise = Graduated
study Graduated = Graduated

-- Ex 7: UpDown counter with two constructors, zero, get, tick, toggle

data UpDown = Up Int | Down Int
  deriving Show

zero :: UpDown
zero = Up 0

get :: UpDown -> Int
get (Up n) = n
get (Down n) = n

tick :: UpDown -> UpDown
tick (Up n) = Up (n + 1)
tick (Down n) = Down (n - 1)

toggle :: UpDown -> UpDown
toggle (Up n) = Down n
toggle (Down n) = Up n

-- Ex 8: Color datatype with rgb function

data Color = Red | Green | Blue | Mix Color Color | Invert Color
  deriving Show

rgb :: Color -> [Double]
rgb Red = [1,0,0]
rgb Green = [0,1,0]
rgb Blue = [0,0,1]
rgb (Mix c1 c2) = zipWith avg (rgb c1) (rgb c2)
  where avg x y = (x + y) / 2
rgb (Invert c) = map (1 -) (rgb c)

-- Ex 9: OneOrTwo parameterized datatype with One and Two constructors

data OneOrTwo a = One a | Two a a
  deriving Show

-- Ex 10: KeyVals datatype with Empty and Pair constructors, toList and fromList

data KeyVals k v = Empty | Pair k v (KeyVals k v)
  deriving Show

toList :: KeyVals k v -> [(k,v)]
toList Empty = []
toList (Pair k v rest) = (k,v) : toList rest

fromList :: [(k,v)] -> KeyVals k v
fromList [] = Empty
fromList ((k,v):xs) = Pair k v (fromList xs)

-- Ex 11: Nat datatype Peano numbers with fromNat and toNat functions

data Nat = Zero | PlusOne Nat
  deriving (Show,Eq)

fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (PlusOne n) = 1 + fromNat n

toNat :: Int -> Maybe Nat
toNat z
  | z < 0 = Nothing
  | z == 0 = Just Zero
  | otherwise = fmap PlusOne (toNat (z - 1))

data Bin = End | O Bin | I Bin
  deriving (Show, Eq)

-- increment a binary number by one
inc :: Bin -> Bin
inc End = I End
inc (O b) = I b
inc (I b) = O (inc b)

prettyPrint :: Bin -> String
prettyPrint End = ""
prettyPrint (O b) = prettyPrint b ++ "0"
prettyPrint (I b) = prettyPrint b ++ "1"

fromBin :: Bin -> Int
fromBin = fromBinHelper 1
  where
    fromBinHelper _ End = 0
    fromBinHelper weight (O b) = 0 + fromBinHelper (weight * 2) b
    fromBinHelper weight (I b) = weight + fromBinHelper (weight * 2) b

toBin :: Int -> Bin
toBin n
  | n <= 0    = O End  -- represent zero explicitly as a single zero digit
  | otherwise = toBinHelper n
  where
    toBinHelper 0 = End
    toBinHelper x
      | even x    = O (toBinHelper (x `div` 2))
      | otherwise = I (toBinHelper (x `div` 2))
