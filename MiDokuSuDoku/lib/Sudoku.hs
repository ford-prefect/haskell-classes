module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.List ((\\), intercalate, nub, sortBy)
import Data.Maybe (catMaybes, isNothing)
import Data.Monoid (First(..))

data Cell = Fixed Int | OneOf [Int] deriving (Eq, Show)

type Row = [Cell]

type Board = [Row]

emptyCell :: Cell
emptyCell = OneOf [1..9]

emptyRow :: Row
emptyRow = replicate 9 emptyCell

emptyBoard :: Board
emptyBoard = replicate 9 emptyRow

getRow :: Board -> Int -> [Cell]
getRow b i = b !! i

getColumn :: Board -> Int -> [Cell]
getColumn b i = map (!! i) b

getBlock :: Board -> (Int, Int) -> [Cell]
getBlock b (r, c) =
  let rows = take 3 . drop (3 * r) $ b
  in concatMap (take 3 . drop (3 * c)) rows

allGroups :: Board -> [[Cell]]
allGroups b =
  let rows   = map (getRow b) [0..8]
      cols   = map (getColumn b) [0..8]
      blocks = map (getBlock b) [ (r, c) | r <- [0..2], c <- [0..2] ]
  in
      rows ++ cols ++ blocks
prettyBoard :: Board -> String
prettyBoard board = intercalate "\n" $
                      map prettyRow (take 3 board) ++
                      [replicate 21 '-'] ++
                      map prettyRow (take 3 . drop 3 $ board) ++
                      [replicate 21 '-'] ++
                      map prettyRow (take 3 . drop 6 $ board)
  where
    prettyCell (Fixed v) = show v
    prettyCell _         = "."

    prettyRow row = prettyRowBlock (take 3 row           ) ++ " | " ++
                    prettyRowBlock (take 3 . drop 3 $ row) ++ " | " ++
                    prettyRowBlock (take 3 . drop 6 $ row)

    prettyRowBlock = unwords . map prettyCell

-- "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
readBoard :: String -> Maybe Board
readBoard b =
  let mBoard = map readRow . chunksOf 9 $ b
  in
    if length b /= 81 || any isNothing mBoard
    then Nothing
    else Just . pruneBoard . catMaybes $ mBoard
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

getCell :: Board -> (Int, Int) -> Cell
getCell board (r, c) = (board !! r) !! c

setCell :: Board -> (Int, Int) -> Cell -> Board
setCell board (r, c) newCell = 
  let oldRow = board !! r
      newRow = replace oldRow c newCell
  in
    replace board r newRow

replace :: [a] -> Int -> a -> [a]
replace l pos new = take pos l ++ [new] ++ drop (pos +1) l

isFinished :: Board -> Bool
isFinished = all isFixed . concat

pruneBoard :: Board -> Board
pruneBoard b =
  let 
    newB = foldl pruneCell b allCells
  in
    if b == newB
    then b
    else pruneBoard newB
  where
    allCells = [ (r, c) | r <- [0..8], c <- [0..8] ]

    pruneCell board (r, c) =
      let inputCells  = getRow board r ++
                        getColumn board c ++
                        getBlock board (r `quot` 3, c `quot` 3)
          fixedValues = nub [ v | Fixed v <- inputCells ]
          oldCell     = getCell board (r, c)
          newCell     = pruneCell' oldCell fixedValues
      in
        setCell board (r, c) newCell
      where
        pruneCell' (Fixed f) _      = Fixed f
        pruneCell' (OneOf vs) fixed = let rem = vs \\ fixed
                                      in if length rem == 1
                                         then Fixed . head $ rem
                                         else OneOf rem

isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _         = False

isValid :: Board -> Bool
isValid b = noEmptyOneOf && allUniqueFixed
  where
    noEmptyOneOf = OneOf [] `notElem` concat b
    allUniqueFixed = all (unique . sortBy sortFixed . filter isFixed) (allGroups b)

    unique [] = True
    unique [x] = True
    unique (x:xs) = x /= head xs && unique xs

    sortFixed (Fixed x) (Fixed y) = compare x y

openPositions :: Board -> [(Int, Int)]
openPositions b =
  let
    positions = [(r, c) | r <- [0..8], c <- [0..8], not . isFixed $ getCell b (r, c)]
  in
    sortBy lessOpen positions
  where
    cellLength pos     = case getCell b pos of
                              OneOf vs -> length vs
                              Fixed _  -> 0 -- never reached
    lessOpen pos1 pos2 = compare (cellLength pos1) (cellLength pos2)

makeAGuess :: Board -> Maybe Board
makeAGuess board =
  case openPositions board of
       []        -> Just board
       positions -> getFirst . mconcat $ concatMap fixOne positions
  where
    fixOne pos =
      let
        OneOf vs  = getCell board pos
        boards    = map (setCell board pos . Fixed) vs
      in
        map (First . solve . pruneBoard) boards

solve :: Board -> Maybe Board
solve board
  | not (isValid board) = Nothing
  | isFinished board    = Just board
  | otherwise           = makeAGuess board
