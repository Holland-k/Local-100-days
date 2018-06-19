bubble :: (Ord a) => [a] -> [a]
bubble (x : y : xs) 
    | x > y = y : bubble(x:xs)
    | otherwise = x : bubble(y:xs)
bubble (x) = (x)


bubble_sort xs i 
    | i == (length xs) = xs
    | otherwise = bubble_sort (bubble xs) (i+1)

bubblesort xs = bubble_sort xs 0

--main = putStrLn(str(bubblesort [5,3,2,6,9,1]))