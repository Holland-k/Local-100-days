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
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr x = case parseExp Lit Add Mul x of 
    (Just a) -> Just (eval a)
    Nothing -> Nothing

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"