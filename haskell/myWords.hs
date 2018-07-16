myWords :: String -> [String]
myWords "" = []
myWords x = takeWhile (/= ' ') x : myWords (dropWhile (== ' ') (dropWhile (/= ' ') x))

myLines :: String -> [String] 
myLines "" = []
myLines x = takeWhile (/= '\n') x : myLines (dropWhile (== '\n') (dropWhile (/= '\n') x))

myBreak :: Char -> String -> [String]
myBreak f "" = []
myBreak f x = takeWhile (/= f) x : myBreak f (dropWhile (== f) (dropWhile (/= f) x))