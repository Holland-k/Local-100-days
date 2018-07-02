-- A.2

-- problem 3
naa = a `div` length xs 
    where 
        a = 10
        xs = [1,2,3,4,5]

-- problem 4
my_last = head(reverse([1..5]))

-- problem 5

my_init_1 = reverse(tail(reverse([1..5])))

my_init_2 = take (length([1..5]) - 1) [1..5]

-- A.4

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

odds n = map (\x -> x*2 + 1) [0..n-1]
const x = \_ -> x

my_fun (#) = \x -> (\y -> x # y)

-- problem 1
halve x = (take n x, drop n x)
    where n = length x `div` 2

-- problem 2
third1 xs =  head(tail(tail(xs)))

third2 xs = xs !! 2

third3 (_:_:x:xs) = x

-- problem 3

safetail1 xs = if null xs then [] else tail xs

safetail2 xs | null xs     = []
    | otherwise             = tail xs


safetail3 []        = []
safetail3 (_:xs)    = xs 

-- Exercises 5.7

-- problem 1

sumsq n = sum [x^2 | x <- [1..n]]

-- problem 2

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(n,m) | n <- [0..x], m <- [0..y]]

-- problem 3

square :: Int -> [(Int,Int)]
square a = [(b,c) | (b,c) <- grid a a, b /= c]

-- problem 4

replicate :: Int -> a -> [a]
replicate n m = [m | _ <- [1..n]]

-- problem 5

pyths :: Int -> [(Int, Int, Int)]
pyths a = [(m,n,p) | m <- [1..a], n <- [1..a], p <- [1..a], m^2 + n^2 == p^2]

-- problem 6

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects x = [y | y <- [1..x], sum (factors y) - y == y]

-- problem 9

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [a*b | (a,b) <- zip xs ys]

-- Exercises Ch 6

-- problem 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)


-- problem 4
euclid :: Int -> Int -> Int
euclid a 0 = 0
euclid 0 b = 0
euclid a b | a == b = a
    | a > b = euclid (a - b) b 
    | otherwise = euclid a (b - a)

-- Chapter 7

-- problem 2

my_all a = and . map a

-- my_any :: (a -> Bool) -> [Bool] -> Bool
my_any a = or . map a

-- my_takeWhile :: (a -> Bool) -> [Bool] -> Bool
my_takeWhile a = filter a

my_dropWhile a [] = []
my_dropWhile a (x:xs) | a x = my_dropWhile a xs
    | otherwise = x:my_dropWhile a xs

-- Problem 3

mymap f = foldr (\x xs -> f x : xs) []

myfilter f = foldr (\x xs -> if f x then x:xs else xs) []

-- Problem 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- Problem 9
-- altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap a b = foldr (\x xs -> if odd((length xs)+1) then (b x) : xs else (a x) : xs) []

-- Problem 10
luhnDouble :: Int -> Int
luhnDouble x = if y > 9 then y - 9 else y 
    where y = x * 2

luhnSum :: [Int] -> Int
luhnSum [] = 0
luhnSum x = head x + (luhnSum (tail x))

luhnCalc :: [Int] -> [Int]
luhnCalc xs = if odd (length xs) 
    then altMap luhnDouble (+0) xs -- Amex
    else altMap (+0) luhnDouble (reverse xs) -- Others

luhn :: [Int] -> Bool
luhn x = if y `mod` 10 == 0 then True else False
    where y = luhnSum (luhnCalc x)

-- Chapter 8
data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop

type Assoc k v = [(k,v)]

type Subst = Assoc Char Bool

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) (bools (n-1)) ++ map (True:) (bools (n-1))

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = (vars p) ++ (vars q)
vars (Imply p q) = vars p ++ vars q

substs :: Prop -> [Subst]
substs p = map (zip vrs) (bools (length vrs))
    where vrs = rmdups (vars p)

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = (eval s p) && (eval s q)
eval s (Imply p q) = (eval s p) <= (eval s q)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]