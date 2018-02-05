module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (asum)
import Data.List ((\\), elemIndices, intercalate, intersect, sortBy)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Monoid (First(..))
import Data.Map.Lazy ((!))

import qualified Data.IntSet as IS
import qualified Data.Map.Lazy as M

import Debug.Trace

data Cell = Fixed Int | OneOf IS.IntSet deriving (Eq, Ord, Show)

type Position = (Int, Int)

allPositions = [ (r, c) | r <- [0..8], c <- [0..8] ]

type Board = M.Map Position Cell

emptyCell :: Cell
emptyCell = OneOf . IS.fromList $ [1..9]

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

pruneGroup :: Board -> (Board -> M.Map Position Cell) -> Board
pruneGroup b getGroup = M.unionWith intersectCells b prunedGroup
  where
    group       = getGroup b
    prunedGroup = M.map pruneCell group

    groupList = M.elems group
    fixeds    = IS.fromList [ v | Fixed v <- groupList ]
    clumps n  = [ vs | OneOf vs <- groupList,
                       IS.size vs == n,
                       length (elemIndices (OneOf vs) groupList) == n ]
    allClumps = clumps 2 ++ clumps 3
    all       = IS.unions $ fixeds : allClumps

    twoOptions (Fixed _)  = False
    twoOptions (OneOf vs) = IS.size vs == 2

    pruneCell (Fixed v) = Fixed v
    pruneCell (OneOf vs) =
      let
        del = if vs `elem` allClumps
              then fixeds
              else all
      in
        newCell $ vs `IS.difference` del

    intersectCells (OneOf vs1) (OneOf vs2) = newCell $ vs1 `IS.intersection` vs2
    intersectCells (Fixed v1)  (Fixed v2)  = if v1 == v2 then Fixed v1 else OneOf IS.empty
    intersectCells (Fixed v1)  _           = Fixed v1
    intersectCells _           (Fixed v2)  = Fixed v2

    newCell options = if IS.size options == 1
                      then Fixed . IS.findMin $ options
                      else OneOf options

pruneBoard :: Board -> Board
pruneBoard b =
  let
    getGroups = map (flip getRow) [0..8] ++
                map (flip getColumn) [0..8] ++
                map (flip getBlock) [ (r, c) | r <- [0..2], c <- [0..2] ]
    newB      = foldl pruneGroup b getGroups
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
    noEmptyOneOf   = OneOf IS.empty `notElem` b
    allUniqueFixed = all (unique . M.filter isFixed) . allGroups $ b

    unique g = (IS.size . IS.fromList . M.elems . M.map fixedToInt $ g) == M.size g
    fixedToInt (Fixed v) = v

openPositions :: Board -> [(Int, Int)]
openPositions b =
  let
    positions = [(r, c) | r <- [0..8], c <- [0..8], not . isFixed $ getCell b (r, c)]
  in
    sortBy lessOpen positions
  where
    cellLength pos     = case getCell b pos of
                              OneOf vs -> IS.size vs
                              Fixed _  -> 0 -- never reached
    lessOpen pos1 pos2 = compare (cellLength pos1) (cellLength pos2)

makeAGuess :: Int -> Board -> Maybe Board
makeAGuess n board =
   let opens     = openPositions board
       boards    = concatMap fixOne opens
       solutions = trace (show (head opens, length opens, length boards, getCell board (head opens))) $
                   map (solve (n+1) . pruneBoard) boards
   in asum solutions
  where
    fixOne pos = let OneOf vs = getCell board pos
                 in map (setCell board pos . Fixed) . IS.toList $ vs

solve :: Int -> Board -> Maybe Board
solve n board
  | not (isValid board) = Nothing
  | isFinished board    = Just board
  | otherwise           = traceShow n $ makeAGuess n board
