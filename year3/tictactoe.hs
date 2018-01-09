module TicTacToe where

data Player = X | O deriving (Eq, Show)
data Cell = Played Player | Empty deriving (Eq, Show)
data Board = Board { size      :: Int
                   , winLength :: Int
                   , rows      :: [[Cell]]
                   } deriving (Show)

newBoard :: Int -> Int -> Board
newBoard s w = Board s w (replicate s (replicate s Empty))

allLocations :: Board -> [(Int, Int)]
allLocations (Board s _ _) = [(x, y) | x <- [0 .. s-1], y <- [0 .. s-1]]

printBoard :: Board -> String
printBoard (Board _ _ [])   = ""
printBoard (Board s w (r:rs)) = printRow r
                              ++ "\n\n"
                              ++ printBoard (Board s w rs)
  where
    printRow []     = ""
    printRow (c:cs) = printCell c ++ "  " ++ printRow cs

    printCell v = case v of
                        Played X -> "x"
                        Played O -> "o"
                        _ -> "~"

invalidCell :: Board -> Int -> Int -> Bool
invalidCell (Board s _ _) x y =
  x < 0 || x >= s || y < 0 || y >= s

getCell :: Board -> Int -> Int -> Cell
getCell b@(Board _ _ rows) x y =
  if invalidCell b x y
  then error "Tried to read more than the size of the board"
  else (rows !! y) !! x

gameOver :: Board -> Bool
gameOver b@(Board _ w rows) =
  any checkBoard (allLocations b) ||
  all (notElem Empty) rows
  where
    checkBoard (x, y) =
      if getCell b x y == Empty
      then False
      else checkE  (w - 1) (getCell b x y) (x + 1) y       ||
           checkS  (w - 1) (getCell b x y) x       (y + 1) ||
           checkSE (w - 1) (getCell b x y) (x + 1) (y + 1) ||
           checkSW (w - 1) (getCell b x y) (x - 1) (y + 1)

    checkE w v x y =
      if w == 0
      then True
      else if invalidCell b x y
           then False
           else getCell b x y == v && checkE (w - 1) v (x + 1) y

    checkS w v x y =
      if w == 0
      then True
      else if invalidCell b x y
           then False
           else getCell b x y == v && checkS (w - 1) v x (y + 1)

    checkSE w v x y =
      if w == 0
      then True
      else if invalidCell b x y
           then False
           else getCell b x y == v && checkSE (w - 1) v (x + 1) (y + 1)

    checkSW w v x y =
      if w == 0
      then True
      else if invalidCell b x y
           then False
           else getCell b x y == v && checkSW (w - 1) v (x - 1) (y + 1)

validMove :: Board -> Player -> Int -> Int -> Bool
validMove b p x y
  | invalidCell b x y      = False
  | getCell b x y /= Empty = False
  | otherwise              = True

replace :: [a] -> Int -> a -> [a]
replace l i x = take i l ++ [x] ++ drop (i + 1) l

find :: (a -> Bool) -> [a] -> a
find pred [] = error "Could not find a valid move"
find pred (x:xs) = if pred x then x else find pred xs

makeMove :: Board -> Player -> Int -> Int -> Board
makeMove b@(Board s w rows) p x y =
  if validMove b p x y
  then Board s w (replace rows y newRow)
  else error "Invalid move"
  where
    newRow   = replace (rows !! y) x (Played p)

-- FIXME: will crash if no valid moves
findMove :: Board -> Player -> (Int, Int)
findMove b p = find (uncurry (validMove b p)) (allLocations b)

playAI :: Board -> Player -> Board
playAI b p = uncurry (makeMove b p) (findMove b p)

playGame :: Board
playGame = playGame' (newBoard 3 3) X
  where
    playGame' b p = if gameOver b
                    then b
                    else playGame' (playAI b p) (opponent p)
    opponent X = O
    opponent O = X
