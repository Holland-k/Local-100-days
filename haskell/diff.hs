import System.IO
import System.Environment
import Data.Set 


diffcheck l1 l2 
    | l1 == l2 = []
    | otherwise = l1 ++ " <==> " ++ l2

diffline [] ys = ys
diffline xs [] = xs
diffline (x:xs) (y:ys) = (diffcheck x y) : (diffline xs ys)

diffcompare fa fb = 
    diffline m n
        where 
            m = fromList (lines fa)
            n = fromList (lines fb)

main = do 
    [fa,fb] <- getArgs
    ta <- readFile fa
    tb <- readFile fb
    (diffcompare ta tb)
    print((show fa) ++ " " ++ (show fb))