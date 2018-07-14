module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys = Just (sum . map (\(x,y) -> if x == y then 0 else 1) $ zip xs ys)
