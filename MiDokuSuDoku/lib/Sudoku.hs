module Sudoku where

import Data.Bits ((.&.), complement, countTrailingZeros, popCount, setBit, testBit)
import Data.Char (digitToInt, isDigit)
import Data.Foldable (asum)
import Data.List (intercalate, minimumBy)
import Data.Maybe (isNothing, mapMaybe)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

newtype Options = Options Int deriving (Eq, Ord)

instance Show Options where
  show = show . optionsToList

optionsToList :: Options -> [Int]
optionsToList (Options o) = filter (testBit o) [1..9]

type Position = (Int, Int)

data Cell = Fixed Int | OneOf Options deriving (Eq, Ord, Show)

type Board = V.Vector Cell
type Group = V.Vector (Int, Cell)

allOptions :: Int
allOptions = foldl setBit 0 [1..9]

emptyCell :: Cell
emptyCell = OneOf . Options $ allOptions

emptyBoard :: Board
emptyBoard = V.replicate 81 emptyCell

positionToIndex :: Position -> Int
positionToIndex (r, c) = r * 9 + c

getCell :: Board -> Position -> Cell
getCell b p = b V.! positionToIndex p

setCell :: Board -> Position -> Cell -> Board
setCell b p cell = V.modify (\v -> MV.write v (positionToIndex p) cell) b

getRow :: Board -> Int -> Group
getRow b r = V.imap (\i v -> (r*9 + i, v)) row
  where
    row = V.take 9 . V.drop (r * 9) $ b

getColumn :: Board -> Int -> Group
getColumn b c = V.imap (\i v -> (i*9 + c, v)) col
  where
    col = V.ifilter (\i _ -> i `mod` 9 == c) b

getBlock :: Board -> (Int, Int) -> Group
getBlock b p@(r, c) = V.imap (\i v -> (cellPos i, v)) block
  where
    cellPos i = r*3*9 + c*3 + i `mod` 3 + (i `div` 3)*9
    block = V.ifilter (\i _ -> p == blockPos i) b
    blockPos i = ((i `div` 9) `quot` 3, (i `mod` 9) `quot` 3)

allGroups :: Board -> [Group]
allGroups b =
  let rows   = map (getRow b) [0..8]
      cols   = map (getColumn b) [0..8]
      blocks = map (getBlock b) [ (r, c) | r <- [0..2], c <- [0..2] ]
  in
      rows ++ cols ++ blocks

prettyBoard :: Board -> String
prettyBoard board = intercalate "\n" $
                      map (prettyRow . V.toList . snd . V.unzip . getRow board) [0..2] ++
                      divider ++
                      map (prettyRow . V.toList . snd . V.unzip . getRow board) [3..5] ++
                      divider ++
                      map (prettyRow . V.toList . snd . V.unzip . getRow board) [6..8]
  where
    prettyCell (Fixed v) = show v
    prettyCell _         = "."

    prettyRow row = prettyRowBlock (take 3 row           ) ++ " | " ++
                    prettyRowBlock (take 3 . drop 3 $ row) ++ " | " ++
                    prettyRowBlock (take 3 . drop 6 $ row)

    prettyRowBlock = unwords . map prettyCell

    divider = [replicate 6 '-' ++ "+" ++ replicate 7 '-' ++ "+" ++ replicate 6 '-']

-- "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
readBoard :: String -> Maybe Board
readBoard b =
  let allCells = mapMaybe readCell b
  in
    if length allCells /= 81
    then Nothing
    else Just . V.fromList $ allCells
  where
    readCell c
      | c == '.'              = Just emptyCell
      | isDigit c && c /= '0' = Just . Fixed . digitToInt $ c
      | otherwise             = Nothing

getFixed :: Cell -> Int
getFixed (Fixed f) = f
getFixed (OneOf _) = error "Unexpected OneOf"

isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _         = False

isFinished :: Board -> Bool
isFinished = isNothing . V.find (not . isFixed)

isValid :: Board -> Bool
isValid b = noEmptyOneOf && allUniqueFixed
  where
    noEmptyOneOf   = OneOf (Options 0) `notElem` b
    allUniqueFixed = all (unique . V.filter (isFixed . snd)) . allGroups $ b

    zero     = 0 :: Int
    unique g = (popCount . V.foldl setBit zero . V.map (getFixed . snd) $ g) == V.length g

pruneGroup :: Board -> (Board -> Group) -> Board
pruneGroup b getGroup = V.update b prunedGroup
  where
    group       = getGroup b
    prunedGroup = V.map pruneCell group

    fixeds      = V.map (getFixed . snd) . V.filter (isFixed . snd) $ group
    --clumps n  = [ vs | OneOf vs <- groupList,
    --                   IS.size vs == n,
    --                   lngth (elemIndices (OneOf vs) groupList) == n ]
    --allClumps = clumps 2 ++ clumps 3
    --all       = IS.unions $ fixeds : allClumps

    pruneCell (i, Fixed v)  = (i, Fixed v)
    pruneCell (i, OneOf (Options vs)) =
      let
        del = V.foldl setBit 0 fixeds
      in
        (i, newCell $ vs .&. complement del .&. allOptions)

    newCell opts = if popCount opts == 1
                   then Fixed . countTrailingZeros $ opts
                   else OneOf . Options $ opts

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

pickPosition :: Board -> (Int, Int)
pickPosition b =
  let
    positions = [(r, c) | r <- [0..8], c <- [0..8], not . isFixed $ getCell b (r, c)]
  in
    minimumBy lessOpen positions
  where
    cellLength pos     = case getCell b pos of
                              OneOf (Options vs) -> popCount vs
                              Fixed _            -> error "Unexpected Fixed"
    lessOpen pos1 pos2 = compare (cellLength pos1) (cellLength pos2)

makeAGuess :: Board -> Position -> Maybe Board
makeAGuess board pos = asum solutions
  where
    (OneOf v) = getCell board pos
    values    = optionsToList v
    boards    = map (setCell board pos . Fixed) values
    solutions = map solve boards

solve :: Board -> Maybe Board
solve b
  | not (isValid board) = Nothing
  | isFinished board    = Just board
  | otherwise           = makeAGuess board (pickPosition board)
  where
    board = pruneBoard b
