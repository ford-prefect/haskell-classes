module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.List ((\\), intercalate, nub, sortBy)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Monoid (First(..))
import Data.Map ((!))
import qualified Data.Map as M

data Cell = Fixed Int | OneOf [Int] deriving (Eq, Show)

type Position = (Int, Int)

allPositions = [ (r, c) | r <- [0..8], c <- [0..8] ]

type Board = M.Map Position Cell

emptyCell :: Cell
emptyCell = OneOf [1..9]

emptyBoard :: Board
emptyBoard = M.fromList . zip allPositions . repeat $ emptyCell

getRow :: Board -> Int -> [Cell]
getRow b r = map (\c -> b ! (r, c)) [0 .. 8]

getColumn :: Board -> Int -> [Cell]
getColumn b c = map (\r -> b ! (r, c)) [0 .. 8]

getBlock :: Board -> (Int, Int) -> [Cell]
getBlock b (r, c) = map (\p -> b ! p) positions
  where
    positions = [ (r, c) | r <- take 3 [r*3..], c <- take 3 [c*3..] ]

allGroups :: Board -> [[Cell]]
allGroups b =
  let rows   = map (getRow b) [0..8]
      cols   = map (getColumn b) [0..8]
      blocks = map (getBlock b) [ (r, c) | r <- [0..2], c <- [0..2] ]
  in
      rows ++ cols ++ blocks

prettyBoard :: Board -> String
prettyBoard board = intercalate "\n" $
                      map (prettyRow . getRow board) [0..2] ++
                      [replicate 21 '-'] ++
                      map (prettyRow . getRow board) [3..5] ++
                      [replicate 21 '-'] ++
                      map (prettyRow . getRow board) [6..8]
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
  let allCells = concat . mapMaybe readRow . chunksOf 9 $ b
  in
    if length allCells /= 81
    then Nothing
    else Just . pruneBoard . M.fromList . zip allPositions $ allCells
  where
    readRow :: String -> Maybe [Cell]
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
getCell board (r, c) = board ! (r, c)

setCell :: Board -> (Int, Int) -> Cell -> Board
setCell board (r, c) newCell = M.insert (r, c) newCell board

isFinished :: Board -> Bool
isFinished = all isFixed

pruneBoard :: Board -> Board
pruneBoard b =
  let 
    newB = foldl pruneCell b allPositions
  in
    if b == newB
    then b
    else pruneBoard newB
  where

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
    noEmptyOneOf = OneOf [] `notElem` b
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
