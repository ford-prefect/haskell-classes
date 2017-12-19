-- the dash dash thing is a comment thing

{-# LANGUAGE FlexibleInstances #-}

module Main where

{-
import Data.Char (toUpper)

upcase :: String -> String
upcase = map toUpper

-- a^2 + b^2
distSquare :: Int -> Int -> Int
distSquare a b =
  -- let square x = x * x
  -- in square a + square b
  square a * square b
  where
    square x = x * x

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = 
  fib (n-1) + fib (n-2)

ma :: (a -> a) -> [a] -> [a]
ma _ [] = []
ma f [x] = [f x]
ma f (x : xs) = f x : ma f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = filter' p [x] ++ filter' p xs

-- homework 1
-- credit card number validation
toDigitsRev :: Integer -> [Integer]
toDigitsRev i
  | i == 0 = []
  | i < 0  = []
  | i > 0  = rem i 10 : toDigits (div i 10)

toDigits :: Integer -> [Integer]
toDigits i = reverse $ toDigitsRev i

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse $ doubleEveryOther' $ reverse l
  where
    doubleEveryOther' [] = []
    doubleEveryOther' [x] = [x]
    doubleEveryOther' (x:y:xs) = x : y * 2 : doubleEveryOther' xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = rem x 10 + div x 10 + sumDigits xs

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther $ toDigits x) == 0

-- List
data LL a = Empty | Cons a (LL a) deriving (Eq, Show)

head' :: LL a -> a
head' Empty = error "Empty list is empty"
head' (Cons a _) = a

-- Exists as Nothing and Just in type Maybe
data Thing a = Notathing | Something a deriving (Show)

safeHead :: LL a -> Thing a
safeHead Empty = Notathing
safeHead list = Something $ head' list

tail' :: LL a -> LL a
tail' Empty = error "Empty list is empty"
tail' (Cons _ rest) = rest
-}

data BST a = EmptyTree | Node a (BST a) (BST a) deriving(Show)
inOrderWalk :: BST a -> [a]
inOrderWalk EmptyTree = []
inOrderWalk (Node a l r) = inOrderWalk l ++ [a] ++ inOrderWalk r

insert :: (Ord a) => BST a -> a -> BST a
insert EmptyTree i = Node i EmptyTree EmptyTree
insert tree@(Node a l r) i
  | a > i = Node a (insert l i) r
  | a < i = Node a l (insert r i)
  | a == i = tree 

present :: (Ord a) => BST a -> a -> Bool
present EmptyTree _ = False
present (Node a l r) i
  | a == i = True
  | a > i = present l i
  | a < i = present r i

fromList :: (Ord a) => [a] -> BST a
fromList = foldl insert EmptyTree

{-
prod :: [Int] -> Int
prod [] = 0
prod [x] = x
prod (x : xs) = x * prod xs

summ :: [Int] -> Int
summ [] = 0
summ (x : xs) = x + summ xs

foldyr :: (t -> r -> r) -> r -> [t] -> r
foldyr _ i [] = i
foldyr f i (x : xs) = f x (foldyr f i xs)

foldy :: (r -> t -> r) -> r -> [t] -> r
foldy _ i [] = i
foldy f i (x:xs) = foldy f (f i x) xs
-}

class Listable a where
  toList :: a -> [Int]

instance Listable [Int] where
  toList = id

instance Listable (Maybe Int) where
  toList Nothing = []
  toList (Just a) = [a]

instance Listable (BST Int) where
  toList = inOrderWalk

main :: IO ()
main = do
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
