{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x - 2) + fib (x - 1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibc :: Integer -> Integer -> [Integer]
fibc a b = a : fibc b (a+b)

fibs2 :: [Integer]
fibs2 = fibc 0 1

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 300 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap b (Cons a as) = Cons (b a) (streamMap b as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = (Cons a) (interleaveStreams bs as)

rulerGen :: Integer -> Stream Integer
rulerGen x = interleaveStreams (streamRepeat x) (rulerGen (x+1))

ruler :: Stream Integer
ruler = rulerGen 0

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons a as) = Cons (a * (-1)) (negate as)
    (+) (Cons a as) (Cons b bs) = Cons (a + b) ((+) as bs)
    (*) (Cons a as) b'@(Cons b bs) = Cons (a*b) (streamMap (*a) bs + as*b')

instance Fractional (Stream Integer) where
    (/) (Cons a as) (Cons b bs) = q 
        where q = Cons (a `div` b) (streamMap (`div` b) (as - q * bs)) 

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)