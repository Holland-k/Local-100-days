fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x - 2) + fib (x - 1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibc :: Integer -> Integer -> [Integer]
fibc a b = a : fibc b (a+b)

fibs2 :: [Integer]
fibs2 = fibc 0 1

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 30 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b