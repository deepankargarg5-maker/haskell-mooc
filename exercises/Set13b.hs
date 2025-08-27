{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Set13b where
import Mooc.Todo
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.IORef
import Data.List
--------------------------------------------------------------------------------
-- Ex 1
-- ifM: runs the first monadic Bool action, then runs opThen if True else opElse
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM opBool opThen opElse = do
  b <- opBool
  if b then opThen else opElse

test :: State Int Bool
test = do
  x <- get
  return (x < 10)

--------------------------------------------------------------------------------
-- Ex 2
-- mapM2: like mapM but for a binary function over two lists (stop at shortest)
mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 _ [] _ = return []
mapM2 _ _ [] = return []
mapM2 op (x:xs) (y:ys) = do
  c <- op x y
  cs <- mapM2 op xs ys
  return (c:cs)

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0.0 = Nothing
safeDiv x y = Just (x / y)

perhapsIncrement :: Bool -> Int -> State Int ()
perhapsIncrement True x = modify (+x)
perhapsIncrement False _ = return ()

--------------------------------------------------------------------------------
-- Ex 3
-- visit marks reachable places using State [String] to track visited
visit :: [(String,[String])] -> String -> State [String] ()
visit maze place = do
  visited <- get
  if place `elem` visited then return ()
  else do
    put (place : visited)
    let neighbors = maybe [] id (lookup place maze)
    mapM_ (visit maze) neighbors

-- path checks if place2 is reachable from place1 via visit and membership check
path :: [(String,[String])] -> String -> String -> Bool
path maze start target = target `elem` visited
  where
    (_, visited) = runState (visit maze start) []

--------------------------------------------------------------------------------
-- Ex 4
-- Use list monad to find triples (i,j,n) where n = i+j, with i,j in ks and n in ns
findSum2 :: [Int] -> [Int] -> [(Int, Int, Int)]
findSum2 ks ns = do
  i <- ks
  j <- ks
  let s = i + j
  guard (s `elem` ns)
  return (i, j, s)

--------------------------------------------------------------------------------
-- Ex 5
-- allSums xs computes all sums from subsets of xs using list monad with inclusion/exclusion
allSums :: [Int] -> [Int]
allSums [] = [0]
allSums (x:xs) = do
  s <- allSums xs
  [s, s + x]

--------------------------------------------------------------------------------
-- Ex 6
-- sumBounded accumulates sums unless result exceeds bound k, then Nothing
sumBounded :: Int -> [Int] -> Maybe Int
sumBounded k xs = foldM (f1 k) 0 xs

f1 :: Int -> Int -> Int -> Maybe Int
f1 k acc x = let s = acc + x in if s <= k then Just s else Nothing

-- sumNotTwice sums only the first occurrence of each Int; state tracks seen values
sumNotTwice :: [Int] -> Int
sumNotTwice xs = fst $ runState (foldM f2 0 xs) []

f2 :: Int -> Int -> State [Int] Int
f2 acc x = do
  seen <- get
  if x `elem` seen
    then return acc
    else put (x:seen) >> return (acc + x)

--------------------------------------------------------------------------------
-- Ex 7
-- Result monad definition with behavior like Maybe but with Failure and NoResult
data Result a = MkResult a | NoResult | Failure String deriving (Show, Eq)

instance Functor Result where
  fmap _ NoResult = NoResult
  fmap _ (Failure s) = Failure s
  fmap f (MkResult x) = MkResult (f x)

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return = MkResult
  NoResult >>= _ = NoResult
  Failure s >>= _ = Failure s
  MkResult x >>= f = f x

--------------------------------------------------------------------------------
-- Ex 8
-- SL monad combining State Int and Logger [String]
data SL a = SL (Int -> (a, Int, [String]))

runSL :: SL a -> Int -> (a, Int, [String])
runSL (SL f) s = f s

msgSL :: String -> SL ()
msgSL m = SL (\s -> ((), s, [m]))

getSL :: SL Int
getSL = SL (\s -> (s, s, []))

putSL :: Int -> SL ()
putSL s' = SL (\_ -> ((), s', []))

modifySL :: (Int -> Int) -> SL ()
modifySL f = SL (\s -> ((), f s, []))

instance Functor SL where
  fmap f (SL g) = SL $ \s -> let (a, s', logs) = g s in (f a, s', logs)

instance Applicative SL where
  pure = return
  (<*>) = ap

instance Monad SL where
  return x = SL $ \s -> (x, s, [])
  (SL f) >>= g = SL $ \s ->
    let (a, s', logs1) = f s
        SL h = g a
        (b, s'', logs2) = h s'
    in (b, s'', logs1 ++ logs2)

--------------------------------------------------------------------------------
-- Ex 9
-- mkCounter produces IO actions inc and get that together count how many times inc was called
mkCounter :: IO (IO (), IO Int)
mkCounter = do
  ref <- newIORef 0
  let inc = modifyIORef' ref (+1)
      get = readIORef ref
  return (inc, get)
