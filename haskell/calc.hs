module Calc where

import ExprT 
import Parser

class Expr a where 
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where 
    lit = Lit 
    add = Add 
    mul = Mul

eval :: ExprT -> Integer
eval (Lit t) = t
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr x = case parseExp Lit Add Mul x of 
    (Just a) -> Just (eval a)
    Nothing -> Nothing

reify :: ExprT -> ExprT
reify = id