-- {-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- {-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main where

import Control.Monad.Free
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity
import Control.Monad.Operational
import Control.Monad.ST (runST)
import Control.Monad.State
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Expr
import MyLib qualified (someFunc)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  print $ Main.eval (Pair (IsZero (Succ (Succ (Main.Lit 1)))) (Main.Lit 1))
  print $ Main.eval (Pair (IsZero (Main.Add (Main.Lit 1) (Main.Lit 20000))) (Str "dsdsa"))
  -- print $ Expr.eval (Expr.Add (Expr.I 123) (Expr.I 2321))
  print $ evalState computation 0
  print $ sumST [1, 2, 3, 4]
  print example1
  print example5
  print example6
  print example7
  print example8
  print example9
  print example10
  print example11
  print example12
  result <- runStateT stackManip [1, 2, 3]
  print result
  print $ runCalc Main.exampleProgram
  evalStateT (replicateM_ 5 incresementAndPrint) 0

type MyStateIO = StateT Int IO

incresementAndPrint :: (MonadIO m) => StateT Int m ()
incresementAndPrint = do
  modify (+ 1)
  n <- get
  liftIO $ putStrLn $ "This is IO action number " ++ show n

data CalcInstr a where
  CAdd :: Int -> Int -> CalcInstr Int
  CMultiply :: Int -> Int -> CalcInstr Int

type CalcProgram = Program CalcInstr

exampleProgram :: CalcProgram Int
exampleProgram = do
  x <- singleton (CAdd 2 3)
  singleton (CMultiply x 4)

runCalc :: CalcProgram a -> a
runCalc = go . view
  where
    go :: ProgramView CalcInstr a -> a
    go (Return a) = a
    go (instr :>>= k) = case instr of
      CAdd x y -> runCalc (k (x + y))
      CMultiply x y -> runCalc (k (x * y))

data TeletypeF x = PutStrln String x | GetLine (String -> x) | ExitSuccess

instance Functor TeletypeF where
  fmap f (PutStrln str x) = PutStrln str (f x)
  fmap f (GetLine k) = GetLine (f . k)
  fmap _ ExitSuccess = ExitSuccess

type Teletype = Free TeletypeF

putStrLn' :: String -> Teletype ()
putStrLn' str = liftF $ PutStrln str ()

getLine' :: Teletype String
getLine' = liftF $ GetLine id

exitSuccess' :: Teletype r
exitSuccess' = liftF ExitSuccess

run :: Teletype r -> IO r
run (Pure r) = return r
run (Free (PutStrln str t)) = putStrLn str >> run t
run (Free (GetLine f)) = getLine >>= run . f
run (Free ExitSuccess) = exitSuccess

echo :: Teletype ()
echo =
  do
    str <- getLine'
    putStrLn' str
    exitSuccess'
    putStrLn' "Finished"

aa =
  do
    x <- getLine
    putStrLn x
    exitSuccess
    putStrLn "Finished"

class MyEq a where
  (==) :: a -> a -> Bool

class (MyEq a) => MyOrd a where
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min :: a -> a -> a

elem :: forall a. (Prelude.Eq a) => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = (x Prelude.== y) || Main.elem x ys

data TurnstileState = Locked | Unlocked
  deriving (Show, Eq)

data TurnstileOutput = Thank | Open | Tut
  deriving (Show, Eq)

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Monad m) => Functor (MaybeT m) where
  fmap = liftM

instance (Monad m) => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (MaybeT m) where
  return = MaybeT . return . Just
  (>>=) :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
      Nothing -> return Nothing
      Just value -> runMaybeT $ f value

type Stack = [Int]

pop :: StateT Stack IO Int
pop = do
  (x : xs) <- get
  put xs
  return x

push :: Int -> StateT Stack IO ()
push x = do
  xs <- get
  put (x : xs)

stackManip :: StateT Stack IO Int
stackManip = do
  push 3
  push 5
  liftIO $ putStrLn "Pushed 3 and 5"
  _ <- pop
  pop

data Weird a b
  = First a
  | Second b
  | Third [(a, b)]
  | Fourth (Weird a b)

weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g
  where
    g (First x) = First (fa x)
    g (Second y) = Second (fb y)
    -- g (Third []) = Third []
    -- g (Third ((x, y) : zs)) = Third ((fa x, fb y) : ((\(Third z) -> z) (g (Third zs))))
    -- g $ Third z = Third (map (\(x, y) -> (fa x, fb y)) z)
    g (Third z) = Third [(fa x, fb y) | (x, y) <- z]
    g (Fourth w) = Fourth (g w)

weirdFold :: (a -> c) -> (b -> c) -> ([(a, b)] -> c) -> (c -> c) -> Weird a b -> c
weirdFold f1 f2 f3 f4 = g
  where
    g (First a) = f1 a
    g (Second b) = f2 b
    g (Third z) = f3 z
    g (Fourth w) = f4 (g w)

example12 :: Box Int
example12 = do
  x <- Box 10
  y <- Box 20
  return (x - y)

example11 :: [(Int, Int)]
example11 = do
  x <- [1, 2, 3]
  y <- [4, 5]
  return (x, y)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x `div` y

lookupValue :: String -> [(String, Int)] -> Maybe Int
lookupValue _ [] = Nothing
lookupValue key ((k, v) : xs) | key Prelude.== k = Just v | otherwise = lookupValue key xs

example10 :: Maybe Int
example10 = do
  x <- lookupValue "a" [("a", 5000), ("b", 10)]
  y <- lookupValue "b" [("a", 5), ("b", 10)]
  safeDiv x y

data Box a where
  Empty :: Box a
  Box :: a -> Box a
  deriving (Show)

instance Functor Box where
  fmap _ Empty = Empty
  fmap f (Box x) = Box (f x)

instance Applicative Box where
  pure = Box
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  Box f <*> Box x = Box (f x)

instance Monad Box where
  return = pure
  Empty >>= _ = Empty
  Box x >>= f = f x

add :: Int -> Int -> Int
add x y = x + y

example1 :: Maybe Int
example1 = Just add <*> Just 3 <*> Just 5

example2 :: Maybe Int
example2 = Just add <*> Just 3 <*> Nothing

example5 :: [Int]
example5 = (add <$> [1, 2]) <*> [3, 4]

example6 :: [Int]
example6 = [(+ 1), (* 2)] <*> [10, 20]

example7 :: Box Int
example7 = Box add <*> Box 10 <*> Box 20

example8 :: Box Int
example8 = Empty <*> Box 10 <*> Box 20

example9 :: Box Int
example9 = Box (* 2) <*> Empty

type Counter = Int

increment :: State Counter ()
increment = modify (+ 1)

decrement :: State Counter ()
decrement = modify (subtract 1)

computation :: StateT Counter Identity Int
computation = do
  increment
  increment
  increment
  decrement
  increment
  increment
  increment
  get

sumST :: [Int] -> Int
sumST xs = runST $ do
  sumRef <- newSTRef 0
  mapM_ (modifySTRef sumRef . (+)) xs
  readSTRef sumRef

fromStoAandS :: Int -> (String, Int)
fromStoAandS c
  | c `mod` 5 Prelude.== 0 = ("foo", c + 1)
  | otherwise = ("bar", c + 1)

data Term a where
  Lit :: Int -> Term Int
  Succ :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If :: Term Bool -> Term a -> Term a -> Term a
  Pair :: Term a -> Term b -> Term (a, b)
  Add :: Term Int -> Term Int -> Term Int
  Sub :: Term Int -> Term Int -> Term Int
  Str :: String -> Term String

eval :: Term a -> a
eval (Main.Lit i) = i
eval (Succ i) = 1 + Main.eval i
eval (IsZero i) = Main.eval i Prelude.== 0
eval (If b e1 e2) = if Main.eval b then Main.eval e1 else Main.eval e2
eval (Pair a b) = (Main.eval a, Main.eval b)
eval (Main.Add a b) = Main.eval a + Main.eval b
eval (Sub a b) = Main.eval a - Main.eval b
eval (Str s) = s
