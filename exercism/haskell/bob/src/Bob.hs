module Bob (responseFor) where

import Data.Char

shouting :: String -> Bool
shouting x = (and $ map (isUpper) $ filter (isAlpha) x) && (or $ map (\y -> isUpper y) x)

question :: String -> Bool
question x = '?' == (head $ dropWhile (isSpace) $ reverse x)

silence :: String -> Bool
silence x = x == "" || (and $ map (isSpace) x)

responseFor :: String -> String
responseFor xs 
    | silence xs = "Fine. Be that way!"
    | shouting xs && question xs = "Calm down, I know what I'm doing!"
    | shouting xs = "Whoa, chill out!"
    | question xs = "Sure."
    | otherwise = "Whatever."
