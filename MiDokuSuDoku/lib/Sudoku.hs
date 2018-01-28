module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.List ((\\), intercalate, intersect, sortBy)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Monoid (First(..))
import Data.Map ((!))

import qualified Data.Map as M
import qualified Data.IntSet as S

data Cell = Fixed Int | OneOf S.IntSet deriving (Eq, Show)

type Position = (Int, Int)

allPositions = [ (r, c) | r <- [0..8], c <- [0..8] ]

type Board = M.Map Position Cell

emptyCell :: Cell
emptyCell = OneOf . S.fromList $ [1..9]

emptyBoard :: Board
emptyBoard = M.fromList . zip allPositions . repeat $ emptyCell

getRow :: Board -> Int -> M.Map Position Cell
getRow b r = M.filterWithKey (\p _ -> r == fst p) b

getColumn :: Board -> Int -> M.Map Position Cell
getColumn b c = M.filterWithKey (\p _ -> c == snd p) b

getBlock :: Board -> (Int, Int) -> M.Map Position Cell
getBlock b p = M.filterWithKey (\pos _ -> p == block pos) b
  where
    block (r, c) = (r `quot` 3, c `quot` 3)

allGroups :: Board -> [M.Map Position Cell]
allGroups b =
  let rows   = map (getRow b) [0..8]
      cols   = map (getColumn b) [0..8]
      blocks = map (getBlock b) [ (r, c) | r <- [0..2], c <- [0..2] ]
  in
      rows ++ cols ++ blocks

prettyBoard :: Board -> String
prettyBoard board = intercalate "\n" $
                      map (prettyRow . rowAsList . getRow board) [0..2] ++
                      [replicate 21 '-'] ++
                      map (prettyRow . rowAsList . getRow board) [3..5] ++
                      [replicate 21 '-'] ++
                      map (prettyRow . rowAsList . getRow board) [6..8]
  where
    rowAsList = map snd . sortBy (\(p1, _) (p2, _) -> compare p1 p2) . M.toList
    prettyCell (Fixed v) = show v
    prettyCell _         = "."

    prettyRow row = prettyRowBlock (take 3 row           ) ++ " | " ++
                    prettyRowBlock (take 3 . drop 3 $ row) ++ " | " ++
                    prettyRowBlock (take 3 . drop 6 $ row)

    prettyRowBlock = unwords . map prettyCell

-- "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
readBoard :: String -> Maybe Board
readBoard b =
  let allCells = mapMaybe readCell b
  in
    if length allCells /= 81
    then Nothing
    else Just . M.fromList . zip allPositions $ allCells
  where
    readCell c
      | c == '.'              = Just emptyCell
      | isDigit c && c /= '0' = Just . Fixed . digitToInt $ c
      | otherwise             = Nothing

getCell :: Board -> (Int, Int) -> Cell
getCell board (r, c) = board ! (r, c)

setCell :: Board -> (Int, Int) -> Cell -> Board
setCell board (r, c) newCell = M.insert (r, c) newCell board

isFinished :: Board -> Bool
isFinished = all isFixed

pruneGroup :: Board -> M.Map Position Cell -> Board
pruneGroup b group = M.unionWith intersectCells b prunedGroup
  where
    prunedGroup = M.map pruneCell group

    del = S.fromList [ v | Fixed v <- M.elems . M.filter isFixed $ group ]

    pruneCell (Fixed v) = Fixed v
    pruneCell (OneOf vs) =
      let rem = vs `S.difference` del
      in
        if S.size rem == 1
        then Fixed . S.findMin $ rem
        else OneOf rem

    intersectCells (OneOf vs1) (OneOf vs2) = OneOf (vs1 `S.intersection` vs2)
    intersectCells (Fixed v1)  (Fixed v2)  = if v1 == v2 then Fixed v1 else OneOf S.empty
    intersectCells (Fixed v1)  _           = Fixed v1
    intersectCells _           (Fixed v2)  = Fixed v2

pruneBoard :: Board -> Board
pruneBoard b =
  let
    newB = foldl pruneGroup b . allGroups $ b
  in
    if b == newB
    then b
    else pruneBoard newB

isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _         = False

isValid :: Board -> Bool
isValid b = noEmptyOneOf && allUniqueFixed
  where
    noEmptyOneOf   = OneOf S.empty `notElem` b
    allUniqueFixed = all (unique . sortBy sortFixed . filter isFixed) groupList
    groupList      = map M.elems . allGroups $ b

    unique []     = True
    unique [x]    = True
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
                              OneOf vs -> S.size vs
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
        boards    = map (setCell board pos . Fixed) . S.toList $ vs
      in
        map (First . solve . pruneBoard) boards

solve :: Board -> Maybe Board
solve board
  | not (isValid board) = Nothing
  | isFinished board    = Just board
  | otherwise           = makeAGuess board
