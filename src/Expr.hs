{-# LANGUAGE GADTs #-}

module Expr where

import Control.Monad.Operational
import Data.Map qualified as Map

data Arithmetic a where
  Add :: Int -> Int -> Arithmetic Int
  Mul :: Int -> Int -> Arithmetic Int
  Lit :: Int -> Arithmetic Int
  Var :: String -> Arithmetic Int
  Let :: String -> Arithmetic Int -> Arithmetic a -> Arithmetic a

type Env = Map.Map String Int

-- Let name expr body ->
--   let val = runArithmeticProgram (eval env (view (k =<< singleton expr)))
--       env' = Map.insert name val env
--    in eval env' (view (k val))

-- let val = eval env (view (expr >>= return))
--     env' = Map.insert name val env
--  in eval env' (view body)

exampleProgram :: Program Arithmetic Int
exampleProgram = do
  x <- singleton $ Add 5 3
  singleton $ Mul 5 x
