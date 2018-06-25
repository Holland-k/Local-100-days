import Data.Char

let2int :: Char -> Bool -> Int
let2int c u | u = ord c - ord 'A'
    | otherwise = ord c - ord 'a'

int2let :: Int -> Bool -> Char
int2let n u | u = chr (ord 'A' + n)
    | otherwise = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let (((let2int c False) + n) `mod` 26) False
    | isUpper c = int2let (((let2int c True) + n) `mod` 26) True
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

