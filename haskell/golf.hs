-- doesn't work properly
skips :: [a] -> [[a]]
skips [] = [[]]
skips a = a : skips [b | (b,c) <- zip a [1..length(a)], even c]

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs) = if (b > a) && (b > c) then b:y else y
	where y = localMaxima(b:c:xs)
localMaxima _ = []