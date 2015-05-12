{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Monoid

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

score :: Char -> Score
score x | elem x "aeilnorstu" = Score 1
score x | elem x "dg" = Score 2
score x | elem x "bcmp" = Score 3
score x | elem x "fhvwy" = Score 4
score 'k' = Score 5
score x | elem x "jx" = Score 8
score x | elem x "qz" = Score 10
score _ = Score 0

scoreString :: String -> Score
scoreString "" = Score 0
scoreString (x:xs) = mappend (score(x)) (scoreString(xs))
