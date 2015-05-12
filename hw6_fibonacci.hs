{-# OPTIONS_GHC -Wall #-}

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fibs1 = map fib [0..]

-- Exercise 2

fibsl :: Integer -> [Integer]
fibsl n
  | n == 0 = [0]
  | n == 1 = [0,1]
  | n > 1 = z ++ [x+y]
  where z = fibsl(n-1)
        (x:y:_) = reverse(z)

fibl :: Integer -> Integer
fibl = last . fibsl

fibs2 = map fibl [0..]

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList(Cons x y) = [x] ++ streamToList(y)

instance Show a => Show (Stream a) where
  show x = show $ take 150 $ streamToList(x)

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f(x)) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f(x)))

-- Exercise 5

nats = streamFromSeed  (+1) 0

streamNats = streamMap (streamRepeat) nats

interleavesStreams :: Stream a -> Stream a -> Stream a
interleavesStreams (Cons x y) ~ (Cons p q) = Cons x ((Cons p) (interleavesStreams y q))

-- ~ above makes the function lazy in its second argument.
-- Without it, ruler stack overflows

ruler = foldr1 (interleavesStreams) (streamToList(streamNats))
