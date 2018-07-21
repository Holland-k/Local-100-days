module Bob (responseFor) where

import Data.Char

shouting :: String -> Bool
shouting x = (all (isUpper) $ filter (isAlpha) x) && (any (\y -> isUpper y) x)

question :: String -> Bool
question x = "?" == (take 1 $ dropWhile (isSpace) $ reverse x)

silence :: String -> Bool
silence x = x == "" || (all (isSpace) x)

responseFor :: String -> String
responseFor xs 
    | silence xs = "Fine. Be that way!"
    | shouting xs && question xs = "Calm down, I know what I'm doing!"
    | shouting xs = "Whoa, chill out!"
    | question xs = "Sure."
    | otherwise = "Whatever."
