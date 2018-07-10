import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . 
    filter even . 
    takeWhile (>1) . 
    iterate (\x -> if even x then x `div` 2 else 3*x+1)

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq, Ord)

treeLevel :: Tree a -> Integer
treeLevel Leaf = -1
treeLevel (Node n _ _ _) = n

insertL :: a -> Tree a -> Tree a
insertL a Leaf = Node 0 Leaf a Leaf
insertL a (Node l left x right) 
    | (treeLevel left) > (treeLevel right) = (Node (treeLevel (insertL a right)+1) left x (insertL a right))
    | otherwise = (Node (treeLevel (insertL a left)+1) (insertL a left) x right)

foldTree :: [a] -> Tree a
foldTree = foldr insertL Leaf

-- xor implemented with map... not fold
xor :: [Bool] -> Bool
xor = odd . sum . map (\y -> if y == True then 1 else 0)

-- trying again with fold
xor2 :: [Bool] -> Bool
xor2 = foldr mxr False 
    
mxr :: Bool -> Bool -> Bool
mxr a b = (a || b) && not (a && b)

mymap :: (a -> b) -> [a] -> [b]
mymap f = foldr (\x y -> f x : y) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [1,2] ++ 
    (map ((+1) . (*2)) $ [1..n] \\
    (map (\(x, y) -> x + y + 2*x*y) .
    filter (\(i, j) -> i+j+2*i*j <= n) $
    cartProd [1..n] [1..n]))

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]