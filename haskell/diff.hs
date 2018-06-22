import System.IO
import System.Environment
import Data.Set 


diffcheck l1 l2 
    | l1 == l2 = []
    | otherwise = l1 ++ " <==> " ++ l2

diffline [] ys = ys
diffline xs [] = xs
diffline (x : xs) (y : ys) =
    (diffcheck x y) : (diffline xs ys)

main = do 
    [fa,fb] <- getArgs
    ta <- readFile fa
    tb <- readFile fb
    let m = lines ta
        n = lines tb
        changes = diffline m n
    print changes