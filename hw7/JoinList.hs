{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ _) | i > 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ 0 (Append _ jl1 _) = indexJ 0 jl1
indexJ i (Append _ x y) | i > 0  && (getSize(size(tag x)) > i) = indexJ i x
                        | otherwise = indexJ (i - getSize(size(tag x))) y

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl | i < 1 = jl
dropJ i jl | i > (getSize(size(tag jl)) - 1) = Empty
dropJ i (Append m x y) | i > (getSize(size(tag x))) = dropJ (getSize(size m) - i) y
                       | otherwise = (dropJ i x) +++ y

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i < 1 = Empty
takeJ i jl | i >= getSize(size(tag jl)) = jl
takeJ i (Append m x y) | i > (getSize(size(tag x))) = x +++ takeJ (getSize(size m) - i) y
                       | otherwise = takeJ i x

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString(s)) s

--instance (Monoid Score, Monoid Size) => Monoid(Score, Size) where
--    mempty = (Score 0, Size 0)
--    mappend (Score a_1, Size b_1) (Score a_2, Size b_2) = (Score (a_1 + a_2), Size(b_1 + b_2))

instance Buffer (JoinList (Score, Size) String) where
    toString (Single _ s) = s
    toString Empty = ""
    toString (Append _ x y) = toString(x) ++ toString(y)
    fromString s = Single ((scoreString(s)), Size 1) s
    line n jl = indexJ n jl
    replaceLine n s jl = (takeJ (n-1) jl) +++ (Single ((scoreString(s)), Size (length(s))) s) +++ (dropJ n jl)
    numLines Empty = 0
    numLines (Single (_,y) _) = getSize(y)
    numLines (Append (_,y) _ _) = getSize(y)
    value Empty = 0
    value (Single (x,_) _)= getScore(x)
    value (Append (x,_) _ _) = getScore(x)
