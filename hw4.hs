import Data.List

-- Exercise 1 --

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map(subtract 2) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


chain :: Integer -> Integer
chain 1 = 1
chain n = if (even n) then n else 3 * n + 1

fun2' :: Integer -> Integer
fun2' n = sum . takeWhile(/=1) $ iterate (chain . (`div` 2)) $ chain n

-- Exercise 2 --

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

addToTree :: Tree a -> a -> Tree a
addToTree Leaf x = Node 0 Leaf x Leaf
addToTree (Node 0 Leaf b Leaf) x = Node 1 (Node 0 Leaf b Leaf) x Leaf
addToTree (Node n t y t') x = if lenTree(t) > lenTree(t')
                              then Node (lenTree(t) + 1) t y (addToTree t' x)
                              else Node (lenTree(t') + 1) (addToTree t x) y t'

lenTree :: Tree a -> Integer
lenTree (Leaf) = 0
lenTree (Node n t x t') = n

foldTree :: [a] -> Tree a
foldTree x = foldl addToTree Leaf x

-- Exercise 3 --

xor :: [Bool] -> Bool
xor x = foldl (/=) False x

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((++) . (\x -> [x]) . f) []

-- Exercise 4 --

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

nxn :: Integer -> [(Integer, Integer)]
nxn n = cartProd [1..n] [1..n]

dropThese :: Integer -> [Integer]
dropThese = map (\(i,j) -> i + j + 2*i*j) . filter(\(i,j) -> i<=j) . nxn

filtered :: Integer -> [Integer]
filtered n = [1..n] \\ dropThese(n)

sieveSundaram :: Integer -> [Integer]
--sieveSundaram n = [2 * x + 1 | x <- ([1..n] \\ dropThese(n))]
sieveSundaram = map (\x -> 2*x+1) . filtered
