module DNA (nucleotideCounts) where

import qualified Data.Map as Map
import Data.List

ntCount :: String -> Char -> (Char, Int)
ntCount xs a = (a, sum $ map (\x -> if a == x then 1 else 0) xs)

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts xs 
    | ((not . and $ map (\x-> elem x "AGCT") xs) && ((length xs) /= 0)) = Left "Bad String"
    | otherwise = Right (Map.fromList (map (ntCount xs) "ACTG"))
