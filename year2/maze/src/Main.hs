{-# LANGUAGE RecordWildCards, DisambiguateRecordFields #-}

module Main where

import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.Environment (getArgs)

data Cell = Cell { cellX :: Int
                 , cellY :: Int } deriving ( Eq, Ord, Show )

data Maze = Maze { mazeW    :: Int
                 , mazeH   :: Int
                 , mazePassages :: M.Map Cell (S.Set Cell) } deriving ( Show )

-- Visit current cell
-- Pick an unvisited neighbour
--   some = Add passage between cell and neighbour
--        = Visit neighbour
--   none = return

mazeHasPassage :: Maze -> Cell -> Cell -> Bool
mazeHasPassage Maze{..} cell1 cell2 = mazeHasPassage' cell1 cell2 || mazeHasPassage' cell2 cell1
  where
    mazeHasPassage' c1 c2 = case M.lookup c1 mazePassages of
      Nothing    -> False
      Just cells -> S.member c2 cells

mazeHasAnyPassage :: Maze -> Cell -> Bool
mazeHasAnyPassage Maze{..} cell = M.member cell mazePassages

mazeAddPassage :: Maze -> Cell -> Cell -> Maze
mazeAddPassage Maze{..} c1 c2 =
  Maze mazeW mazeH
  . M.insertWith S.union c2 (S.singleton c1)
  . M.insertWith S.union c1 (S.singleton c2)
  $ mazePassages

visitCell :: Maze -> Cell -> Maze
visitCell maze cell = foldl' visitCell' maze (getNeighbours cell)
  where
    getNeighbours (Cell x y) = filter (isValid maze) [ Cell (x - 1) y
                                                     , Cell x       (y - 1)
                                                     , Cell (x + 1) y
                                                     , Cell x       (y + 1)]
    isValid Maze{..} (Cell x y) = x >=0 && y >= 0 && x < mazeW && y < mazeH
    visitCell' maze'@Maze{..} neighbour =
      if mazeHasAnyPassage maze' neighbour
      then maze'
      else visitCell (mazeAddPassage maze' cell neighbour) neighbour

generateMaze :: Int -> Int -> Maze
generateMaze w h = visitCell (Maze w h M.empty) (Cell 0 0)

renderMaze :: Maze -> String
renderMaze maze@Maze{..} = concatMap cellToStr [Cell x y | y <- [-1..mazeH-1], x <- [-1..mazeW-1]]
  where
    cellToStr c@(Cell x y)
      | x == -1 && y == -1           = " "                   -- top left
      | x == -1 && y == mazeH-1      = "|"                   -- bottom left
      | x == -1                      = "|"                   -- left wall 
      | x == mazeW-1 && y == -1      = "_ \n"                -- top right
      | x == mazeW-1 && y == mazeH-1 = "_|\n"                -- bottom right
      | y == -1                      = "__"                  -- top wall 
      | x == mazeW-1                 = drawCell c ++ "\n"    -- right wall 
      | otherwise                    = drawCell c
    drawCell c = down c ++ right c
    down c@(Cell x y)  = if mazeHasPassage maze c (Cell x (y + 1)) then " " else "_"
    right c@(Cell x y) = if mazeHasPassage maze c (Cell (x + 1) y) then " " else "|"

main :: IO ()
main = do
  (w : h : _) <- fmap (map read) getArgs
  putStrLn $ renderMaze $ generateMaze w h
