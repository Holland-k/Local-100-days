bubble :: (Ord a) => [a] -> [a]
bubble (x : y : xs) 
    | x > y = y : bubble(x:xs)
    | otherwise = x : bubble(y:xs)
bubble (x) = (x)


bubble_sort xs i 
    | i == (length xs) = xs
    | otherwise = bubble_sort (bubble xs) (i+1)

bubblesort xs = bubble_sort xs 0

merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergesort xs
    | length (xs) > 1 = merge (mergesort (take n xs)) (mergesort (drop n xs)) 
    | otherwise = xs
    where n = (length xs) `div` 2
    