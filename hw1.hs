-- Exercise 1


toDigitsRev :: Integer -> [Integer]
toDigitsRev n = [( n `div` 10^x) `mod` 10 | x <- takeWhile(\y -> n >= 10^y)[0..] ]

toDigits    :: Integer -> [Integer]
toDigits n = reverse(toDigitsRev(n))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:xs) = (x:(2*y):doubleEveryOtherRev(xs))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse(doubleEveryOtherRev(reverse l))

sumDigits :: [Integer] -> Integer
sumDigits l = sum(concatMap toDigits l)

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits(n))) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n pS pD pT
    | n == 1 = [(pS, pD)]
    | n == 2 = [(pS,pT), (pS, pD), (pT,pD)]
    | n > 2  = (hanoi (n-1)  pS pT pD) ++ [(pS, pD)] ++ (hanoi (n-1) pT pD pS)
