module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.List ((\\), intercalate)
import Data.Maybe (catMaybes, isNothing, mapMaybe)

data Cell = Fixed Int | OneOf [Int] deriving (Show)

type Row = [Cell]

type Board = [Row]

emptyCell :: Cell
emptyCell = OneOf [1..9]

emptyRow :: Row
emptyRow = replicate 9 emptyCell

emptyBoard :: Board
emptyBoard = replicate 9 emptyRow

prettyBoard :: Board -> String
prettyBoard = intercalate "\n\n" . map prettyRow
  where
    prettyCell (Fixed v) = show v
    prettyCell _         = "."

    prettyRow = intercalate "  " . map prettyCell

-- "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
readBoard :: String -> Maybe Board
readBoard b =
  let mBoard = map readRow . chunksOf 9 $ b
  in
    if length b /= 81 || any isNothing mBoard
    then Nothing
    else Just . catMaybes $ mBoard
  where
    readRow :: String -> Maybe Row
    readRow r =
      let mRow = map readCell r
      in
        if any isNothing mRow
        then Nothing
        else Just . catMaybes $ mRow

    readCell c
      | c == '.'              = Just emptyCell
      | isDigit c && c /= '0' = Just . Fixed . digitToInt $ c
      | otherwise             = Nothing

    chunksOf n [] = []
    chunksOf n l =
      let first = take n l
          rest = chunksOf n (drop n l)
      in
        first : rest

pruneOneOfs :: [Cell] -> [Cell]
pruneOneOfs cells = map pruneOneOf cells
  where
    del = [ v | Fixed v <- cells ]

    pruneOneOf (Fixed v) = Fixed v
    pruneOneOf (OneOf vs) =
      let rem = vs \\ del
      in
        if length rem == 1
        then Fixed . head $ rem
        else OneOf rem

isFinished :: Board -> Bool
isFinished = undefined

isValid :: Board -> Bool
isValid = undefined

pickACell :: Board -> (Int, Int)
pickACell = undefined

makeAGuess :: Board -> Board
makeAGuess = undefined
