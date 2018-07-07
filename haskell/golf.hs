import Data.List
-- doesn't work properly
skips :: [a] -> [[a]]
skips [] = [[]]
skips a = a : skips [b | (b,c) <- zip a [1..length(a)], even c]

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