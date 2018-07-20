module DNA (nucleotideCounts) where

import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Show, Eq)

toNucleotide :: Char -> Maybe Nucleotide
toNucleotide x 
    | x == 'A' = Just A
    | x == 'C' = Just C
    | x == 'G' = Just G
    | x == 'T' = Just T
    | otherwise = Nothing

ntCount :: String -> Char -> (Char, Int)
ntCount xs a = (a, sum $ map (\x -> if a == x then 1 else 0) xs)

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts xs 
    | ((sequenceA $ fmap toNucleotide xs) == Nothing) && length xs /= 0 = Left "Bad String"
    | otherwise = Right (Map.fromList (map (ntCount xs) "ACTG")) -- still have to return (Map Char Int)
