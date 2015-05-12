{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List
import Data.Ord
import Data.Char

-- Exercise 1 - Hopscotch

skips :: [a] -> [[a]]
take_every m = map snd . filter ((== m) . fst) . zip (cycle [1..m])
skips x = map (\y -> take_every y x) [1..length(x)]

 -- Exercise 2 - Local maxima

localMaxima :: [Int] -> [Int]
localMaxima x =  (map fst . filter snd) (zip x (map (\(a,y,z) -> (a>y && a>z)) (zip3 x (take (length x) (0:x)) (drop 1 x))))

 -- Exercise 3 - Histogram

count :: Eq a => (a, [a]) -> Int
count(x,y) = length(filter(==x)y)

frequencies :: [Int] -> [(Int, Int)]
frequencies y = sort (map (\x -> (x, count(x,y))) y)

maxFrequency :: [Int] -> Int
maxFrequency y = maximum $ map snd $ frequencies(y)

starFrequencies :: [Int] -> [String]
starFrequencies y = map (\x -> ([intToDigit(x)] ++ "= " ++ concat( replicate (count(x,y)) "*" ) ++ concat( replicate (maxFrequency(y) - count(x,y)) " "))) [0..9]

histogram :: [Int] -> String
histogram y = intercalate "\n" (reverse(transpose(starFrequencies(y))))
