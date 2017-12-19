module NumbersAsWords where

import Data.List (unwords)
import Data.Maybe (fromMaybe)

thousands :: [(Int, String)]
thousands = [ (1, "thousand")
            , (2, "million")
            , (3, "billion")
            , (4, "trillion")
            ]

thousandsAsWords :: Int -> String
thousandsAsWords n = fromMaybe undefined (lookup n thousands)

maxThousands :: Int
maxThousands = length thousands

unitsAsWords :: Int -> String
unitsAsWords n = case n of
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"

tensAsWords :: Int -> String
tensAsWords n = case n of
  1 -> "ten"
  2 -> "twenty"
  3 -> "thirty"
  4 -> "forty"
  5 -> "fifty"
  6 -> "sixty"
  7 -> "seventy"
  8 -> "eighty"
  9 -> "ninety"

lttAsWords :: Int -> [String]
lttAsWords n =
  let u = n `mod` 10
      t = n `div` 10 `mod` 10
      h = n `div` 100 `mod` 10
  in
  case (h, t, u) of
    (0, 0, 0) -> ["zero"]
    (0, 0, u) -> [unitsAsWords u]
    (0, 1, 1) -> ["eleven"]
    (0, 1, 2) -> ["twelve"]
    (0, 1, 3) -> ["thirteen"]
    (0, 1, 4) -> ["fourteen"]
    (0, 1, 5) -> ["fifteen"]
    (0, 1, 6) -> ["sixteen"]
    (0, 1, 7) -> ["seventeen"]
    (0, 1, 8) -> ["eighteen"]
    (0, 1, 9) -> ["nineteen"]
    (0, t, 0) -> [tensAsWords t]
    (0, t, u) -> [tensAsWords t, unitsAsWords u]
    (h, 0, 0) -> [unitsAsWords h, "hundred"]
    (h, t, u) -> unitsAsWords h : "hundred" : lttAsWords (t * 10 + u)

numberAsWords' :: Int -> [String]
numberAsWords' n
  | n < 1000          = lttAsWords n
  | p <= maxThousands = concat [ numberAsWords' (n `div` (1000 ^ p))
                               , [thousandsAsWords p]
                               , if rest > 0 then numberAsWords' rest else []
                               ]
  | otherwise         = concat [ numberAsWords' (n `div` (1000 ^ maxThousands))
                               , [thousandsAsWords maxThousands]
                               , if rest' > 0 then numberAsWords' rest' else []
                               ]
  where
    p = floor (logBase 1000 (fromIntegral n))
    rest = n `mod` (1000 ^ p)
    rest' = n `mod` (1000 ^ maxThousands)

numberAsWords :: Int -> String
numberAsWords n
  | n < 0     = "minus" ++ numberAsWords (-n)
  | otherwise = unwords (numberAsWords' n)
