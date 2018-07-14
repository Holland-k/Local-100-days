module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f [] = []
accumulate f xs = f (head xs) : accumulate f (tail xs)
