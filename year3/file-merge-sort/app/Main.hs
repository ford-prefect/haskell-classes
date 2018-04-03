module Main where

import Data.List (sort)
import System.Directory (removeFile)
import System.IO.Temp (writeSystemTempFile)

import Lib

merge :: [Int] -> [Int] -> [Int]
merge a [] = a
merge [] b = b
merge (a:as) (b:bs) = if a < b
                      then a : merge as (b:bs)
                      else b : merge (a:as) bs

chunk :: Int -> [a] -> [[a]]
chunk n [] = [[]]
chunk n l  = let (hd, tl) = splitAt n l
             in hd : chunk n tl

writeNumbers :: [Int] -> IO FilePath
writeNumbers =
  writeSystemTempFile "sort" . unlines . map show . sort

readNumbers :: FilePath -> IO [Int]
readNumbers path = map read . lines <$> readFile' path
  where
    readFile' "-"  = getContents
    readFile' path = readFile path

mergeNumbers :: FilePath -> FilePath -> IO FilePath
mergeNumbers f1 f2 = do
  n1  <- readNumbers f1
  n2  <- readNumbers f2
  out <- writeNumbers (merge n1 n2)
  removeFile f1
  removeFile f2
  return out

halfM :: Monad m => [a] -> (a -> a -> m a) -> m [a]
halfM []  _            = return []
halfM [a] _            = return [a]
halfM (a:b:rest) merge = do
  merged     <- merge a b
  mergedRest <- halfM rest merge
  return $ merged : mergedRest

reduceM :: Monad m => (a -> a -> m a) -> [a] -> m a
reduceM _     []  = error "death"
reduceM _     [a] = return a
reduceM merge l   = halfM l merge >>= reduceM merge

--main :: IO ()
--main = do
--  numbers <- readNumbers "-"
--  files   <- mapM writeNumbers (chunk 10000 numbers)
--  final   <- reduceM mergeNumbers files
--  out     <- readFile final
--  putStr out
--  removeFile final

main :: IO ()
main =
  readNumbers "-"
  >>= mapM writeNumbers . chunk 10000
  >>= reduceM mergeNumbers
  >>= \final -> readFile final
                >>= putStr
                >> removeFile final
