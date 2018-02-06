module Main where

import System.Environment (getArgs)

import Sudoku

main :: IO ()
main = do
  [s] <- getArgs
  case readBoard s of
    Nothing -> putStrLn "Invalid board"
    Just b  -> case solve b of
      Nothing -> putStrLn "No solution found"
      Just b' -> putStrLn $ prettyBoard b'
