module Tests where

import JoinList
import Sized
import Data.Monoid

jl = Append (Size 4)
     (Append (Size 3)
                 (Single (Size 1) 'y')
                 (Append (Size 2)
                             (Single (Size 1) 'e')
                             (Single (Size 1) 'a')))
     (Single (Size 1) 'h')

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
