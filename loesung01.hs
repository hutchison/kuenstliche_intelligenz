isPrime :: Int -> Bool
isPrime n = (n > 1) && ([k | k <- [1..n], n `mod` k == 0] == [1, n])

data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a) deriving Show

t = Branch (Leaf 2) (Branch (Branch (Leaf 13) (Leaf 23)) (Leaf 42))

sumLeaves :: Num a => Tree a -> a
sumLeaves Empty = 0
sumLeaves (Leaf v) = v
sumLeaves (Branch left right) = sumLeaves left + sumLeaves right

mapLeaves :: (a -> a) -> Tree a -> Tree a
mapLeaves f Empty = Empty
mapLeaves f (Leaf v) = Leaf (f v)
mapLeaves f (Branch left right) = Branch (mapLeaves f left) (mapLeaves f right)
