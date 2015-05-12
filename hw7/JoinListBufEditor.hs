module Main where

import Buffer
import Scrabble
import Sized
import JoinList
import Editor
import Tests

buf :: JoinList (Score, Size) String
buf = foldr (+++) Empty $ map fromString
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor $ buf
