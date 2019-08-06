module Hffp where

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two c d) = (a == c) && (b == d)

data StringOrInt = 
    TisAnInt Int 
    | TisAString String

instance Eq StringOrInt where 
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString a) (TisAString b) = a == b
    (==) _ _ = False

data Pair a = Pair a a deriving (Show)

instance Eq a => Eq (Pair a) where
    (==) (Pair b b') (Pair c c') = b == c && b' == c'

data Tuple a b = Tuple a b deriving (Show)

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple c d) (Tuple c' d') = (c == c') && (d == d')

data Which a = 
    ThisOne a
    | ThatOne a deriving (Show)

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne b) (ThisOne c) = b == c
    (==) (ThatOne b) (ThatOne c) = b == c
    (==) _ _ = False

data EitherOr a b = 
    Hello a
    | Goodbye b deriving (Show)

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello c) (Hello c') = (c == c')
    (==) (Goodbye c) (Goodbye c') = (c == c')
    (==) _ _ = False

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Ord)

instance Eq DayOfWeek where 
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected then print "okay" else error "Failed"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right
 
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO()
testPreorder = if preorder testTree == [2,1,3]
    then putStrLn "Okay"
    else putStrLn "Failed"

testInorder :: IO()
testInorder = if inorder testTree == [1,2,3]
    then putStrLn "Okay"
    else putStrLn "Failed"

testPostorder :: IO()
testPostorder = if postorder testTree == [1, 3, 2]
    then putStrLn "Okay"
    else putStrLn "Failed"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f x Leaf = x
foldTree f x (Node left y right) = 
    f y (foldTree f (foldTree f x left) right)

testFold = if foldTree (+) 0 testTree == 6
    then print "okay"
    else print "failed"

main :: ()
main = undefined