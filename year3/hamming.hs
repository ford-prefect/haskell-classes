module Hamming where

hamming :: String -> String -> Int
hamming [] []           = 0
hamming (c1:s1) (c2:s2) = (if c1 /= c2 then 1 else 0) + hamming s1 s2
hamming _ _             = error "hamming distance undefined for unequal lengths"
