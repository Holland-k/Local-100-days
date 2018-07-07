import Data.List
-- doesn't work properly
skip :: [a] -> Int -> [[a]]
skip [] _ = []
skip a n = [head a : d] ++ skip (tail a) (n+1)
    where d = [b | (b,c) <- zip (tail a) [1..((length a) -1)], c `mod` n == 0]

skips :: [a] -> [[a]]
skips [] = [[]]
skips a = [a] ++ skip (tail a) 2

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = if (b > a) && (b > c) then b:y else y
    where y = localMaxima(b:c:xs)
localMaxima _ = []

histGraph :: Int -> String
histGraph n = replicate n '*'

printClean n = (intercalate "\n" (reverse (transpose (map (++ q) n)))) 
    ++ "\n" 
    ++ replicate 10 '=' ++ "\n"
    ++ "0123456789\n" 
    where q = replicate (length (maximum n)) ' '

histogram :: [Integer] -> String
histogram a = printClean [histGraph (length (filter (== n) a)) | n <- [0..9]]