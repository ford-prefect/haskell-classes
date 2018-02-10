module Sudoku where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Bits ((.|.), (.&.), complement, countTrailingZeros, popCount, setBit, testBit)
import Data.Char (digitToInt, isDigit)
import Data.Foldable (asum)
import Data.List ((\\), group, intercalate, minimumBy, sort)
import Data.Maybe (isNothing, fromJust, mapMaybe)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

newtype Options = Options Int deriving (Eq, Ord)

instance Show Options where
  show = show . optionsToList

optionsToList :: Options -> [Int]
optionsToList (Options o) = filter (testBit o) [1..9]

type Position = (Int, Int)

data Cell = Fixed Int | OneOf Options deriving (Eq, Ord)

instance Show Cell where
  show (Fixed v) = "Fixed " ++ show (countTrailingZeros v)
  show (OneOf o) = "OneOf " ++ show o

type Board = V.Vector Cell
type Group = [(Int, Cell)]

allOptions :: Int
allOptions = foldl setBit 0 [1..9]

emptyCell :: Cell
emptyCell = OneOf . Options $ allOptions

emptyBoard :: Board
emptyBoard = V.replicate 81 emptyCell

positionToIndex :: Position -> Int
positionToIndex (r, c) = r * 9 + c

getCell :: Board -> Position -> Cell
getCell b p = b `V.unsafeIndex` positionToIndex p

setCell :: Board -> Position -> Cell -> Board
setCell b p cell = V.modify (\v -> MV.write v (positionToIndex p) cell) b

rows :: [[Int]]
rows = [ [ positionToIndex (r, c) | c <- [0..8] ] | r <- [0..8] ]

columns :: [[Int]]
columns =  [ [ positionToIndex (r, c) | r <- [0..8] ] | c <- [0..8] ]

blocks :: [[Int]]
blocks =  [ [ positionToIndex (r + br*3, c + bc * 3) | r <- [0..2], c <- [0..2] ] | br <- [0..2], bc <- [0..2] ]

getGroup :: Board -> [Int] -> Group
getGroup b is = zip is $ map (V.unsafeIndex b) is

allGroups :: [[Int]]
allGroups = rows ++ columns ++ blocks

prettyBoard :: Board -> String
prettyBoard board = intercalate "\n" $
                      map (prettyRow . map snd . getGroup board . (!!) rows) [0..2] ++
                      divider ++
                      map (prettyRow . map snd . getGroup board . (!!) rows) [3..5] ++
                      divider ++
                      map (prettyRow . map snd . getGroup board . (!!) rows) [6..8]
  where
    prettyCell (Fixed v) = show $ countTrailingZeros v
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
      | isDigit c && c /= '0' = Just . setFixed . digitToInt $ c
      | otherwise             = Nothing

getValue :: Cell -> Int
getValue (Fixed f) = f
getValue (OneOf (Options o)) = o

setFixed :: Int -> Cell
setFixed = Fixed . setBit 0

isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _         = False

isFinished :: Board -> Bool
isFinished = isNothing . V.find (not . isFixed)

isValid :: Board -> Bool
isValid b = noEmptyOneOf && allUniqueFixed
  where
    noEmptyOneOf   = OneOf (Options 0) `notElem` b
    allUniqueFixed = all (unique . filter (isFixed . snd) . getGroup b) allGroups

    unique g = (popCount . foldl (.|.) 0 . map (getValue . snd) $ g) == length g

pruneGroup :: Board -> [Int] -> Maybe Board
pruneGroup b g = V.unsafeUpd b <$> prunedGroup
  where
    grp         = getGroup b g
    prunedGroup = mapM pruneCell grp

    fixeds      = map (getValue . snd)
                . filter (isFixed . snd)
                $ grp

    clumps n    = map head
                . filter (\x -> n == length x)
                . group
                . sort
                $ [ vs | (_, OneOf (Options vs)) <- grp, popCount vs == n ]

    allClumps   = clumps 2 ++ clumps 3

    uniqOption p n = let
                       cellsWithN = filter (flip hasOption n . snd) p
                     in
                       if length cellsWithN == 1
                       then Just . fmap (const . setFixed $ n) . head $ cellsWithN
                       else Nothing

    hasOption (OneOf (Options o)) n = testBit o n
    hasOption (Fixed _) _           = False

    uniques = mapMaybe (uniqOption grp) ([1..9] \\ map countTrailingZeros fixeds)

    pruneCell (i, Fixed v)  = Just (i, Fixed v)
    pruneCell (i, OneOf (Options vs)) =
      let
        elim    = foldl (.|.) 0 $
                   if vs `notElem` allClumps
                   then fixeds ++ allClumps
                   else fixeds
        opts    = vs .&. complement elim .&. allOptions
        mUnique = lookup i uniques
      in
      ((,) i <$> mUnique) <|> case popCount opts of
                                0 -> Nothing
                                1 -> Just (i, Fixed opts)
                                _ -> Just (i, OneOf . Options $ opts)

pruneBoard :: Board -> Maybe Board
pruneBoard b =
  let
    newB      = foldM pruneGroup b allGroups
  in
    case newB of
         Nothing           -> Nothing
         Just b' | b' == b -> Just b
         Just b'           -> pruneBoard b'

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
    boards    = map (setCell board pos . setFixed) values
    solutions = map solve boards

solve :: Board -> Maybe Board
solve b
  | isNothing mBoard    = Nothing
  | not (isValid board) = Nothing
  | isFinished board    = Just board
  | otherwise           = makeAGuess board (pickPosition board)
  where
    mBoard = pruneBoard b
    board  = fromJust mBoard
