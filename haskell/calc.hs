{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT 
import Parser
import StackVM

class Expr a where 
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where 
    lit = Lit 
    add = ExprT.Add 
    mul = ExprT.Mul

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where 
    lit = id
    add = (+)
    mul = (*)    

instance Expr Bool where 
    lit i = if i > 0 then True else False
    add = (||)
    mul = (&&)

instance Expr MinMax where 
    lit = MinMax
    add (MinMax a) (MinMax b) = (MinMax (max a b))
    mul (MinMax a) (MinMax b) = (MinMax (min a b))

instance Expr Mod7 where
    lit a = (Mod7 (a `mod` 7))
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = (Mod7 ((a * b) `mod` 7))

eval :: ExprT -> Integer
eval (Lit t) = t
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr x = case parseExp Lit ExprT.Add ExprT.Mul x of 
    (Just a) -> Just (eval a)
    Nothing -> Nothing

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr StackVM.Program where
    lit i = if i == True || i == False 
        then [StackVM.PushB i]
        else [StackVM.PushI i]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]
    -- and a b = a ++ b ++ [StackVM.And]
    -- or a b = a ++ b ++ [StackVM.Or]

compile :: String -> Maybe Program
compile x = parseExp PushI PushB StackVM.Add StackVM.Mul And Or x 