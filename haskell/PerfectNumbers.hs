module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
    | n == y = Just Perfect
    | y > n = Just Abundant 
    | otherwise = Just Deficient  
    where y = sum $ 1 : [x | x <- [2..(n-1)], n `mod` x == 0]
