{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import StackVM

--Exercise 1--

eval :: ExprT -> Integer
eval (ExprT.Add x y) = eval(x) + eval(y)
eval (ExprT.Mul x y) = eval(x) * eval(y)
eval (Lit n) = n

--Exercise 2--

maybeEval :: Maybe ExprT -> Maybe Integer
maybeEval (Nothing) = Nothing
maybeEval (Just x) = Just (eval(x))

evalStr :: String -> Maybe Integer
--evalStr s = maybeEval $ parseExp Lit ExprT.Add ExprT.Mul s
evalStr s = fmap (eval) (parseExp Lit ExprT.Add ExprT.Mul s)

--Exercise 3--

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit n = Lit n
  mul x y = ExprT.Mul x y
  add x y = ExprT.Add x y

--Exercise 4--

instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)

instance Expr Bool where
  lit = (>0)
  mul = (&&)
  add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax . id
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)

--Exercise 5--

instance Expr Program where
  lit n = [PushI n]
  mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]
  add p1 p2 = p1 ++ p2 ++ [StackVM.Add]

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testProgram = testExp :: Maybe Program

compile :: String -> Maybe Program
compile = parseExp lit add mul

testCompile s = fmap (stackVM) (compile s)
