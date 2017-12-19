{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid

data BinaryTree m a = Empty | Value m a | BinaryTree m (BinaryTree m a) (BinaryTree m a)

instance (Show m, Show a) => Show (BinaryTree m a) where
  show Empty = "\n"
  show (Value m a) = "( " ++ show m ++ " )" ++ show a ++ "\n"
  show (BinaryTree m l r) = show l ++ "[ " ++ show m ++ " ]\n" ++ show r

meta :: Monoid m => BinaryTree m a -> m
meta Empty            = mempty
meta (Value m _)      = m
meta (BinaryTree m _ _) = m

leftTree :: BinaryTree m a -> BinaryTree m a
leftTree Empty              = Empty
leftTree (Value _ _)        = Empty
leftTree (BinaryTree _ l r) = l

instance Monoid m => Monoid (BinaryTree m a) where
  mempty = Empty
  mappend Empty b = b
  mappend a Empty = a
  mappend a b = BinaryTree (meta a <> meta b) a b

instance Foldable (BinaryTree m) where
  foldMap _ Empty            = mempty
  foldMap f (Value _ a)      = f a
  foldMap f (BinaryTree _ l r) = foldMap f l <> foldMap f r

--        8
--    4       4
--  2   2   2   2
-- 1 1 1 1 1 1 1 1
--
-- if l <= n; recurse l n
-- else recurse r (n - left)

newtype LineCount = LineCount { getLineCount :: Sum Int } deriving (Show, Monoid)

newtype WordCount = WordCount { getWordCount :: Sum Int } deriving (Show, Monoid)

data EditorMeta = EditorMeta { lineCount :: LineCount
                             , wordCount :: WordCount } deriving (Show)

instance Monoid EditorMeta where
  mempty = EditorMeta mempty mempty
  mappend (EditorMeta l1 w1) (EditorMeta l2 w2) = EditorMeta (l1 <> l2) (w1 <> w2)

newtype Line = Line { getLine :: String } deriving (Show)

fromString :: String -> Line
fromString = Line

makeValue :: Line -> BinaryTree EditorMeta Line
makeValue line = Value (EditorMeta (LineCount 1) (countLineWords line)) line

countLineWords :: Line -> WordCount
countLineWords (Line l) = WordCount . Sum . length . words $ l

fromList :: [Line] -> BinaryTree EditorMeta Line
fromList []  = Empty
fromList [a] = makeValue a
fromList xs  = fromList l <> fromList r
  where
    half   = div (length xs) 2
    (l, r) = splitAt half xs

toList :: BinaryTree m a -> [a]
toList = foldMap (\x -> [x])

countWords :: BinaryTree EditorMeta a -> WordCount
countWords Empty = mempty
countWords t     = wordCount . meta $ t

countLines :: BinaryTree EditorMeta a -> LineCount
countLines Empty = mempty
countLines t     = lineCount . meta $ t

leftLines :: BinaryTree EditorMeta a -> Int
leftLines = getSum . getLineCount . countLines . leftTree

getAt :: BinaryTree EditorMeta a -> Int -> Maybe a
getAt Empty n       = Nothing
getAt (Value m a) n = if (getSum . getLineCount . lineCount) m == n then Just a else Nothing
getAt t@(BinaryTree _ l r) n
  | n <= leftLines t = getAt l n
  | otherwise        = getAt r (n - leftLines t)

-- FIXME: Doesn't deal with corner cases
insertAt :: BinaryTree EditorMeta Line -> Int -> Line -> BinaryTree EditorMeta Line
insertAt Empty 1 line         = makeValue line
insertAt v@(Value m _) _ line = v <> makeValue line
insertAt t@(BinaryTree _ l r) n line
  | n <= leftLines t = insertAt l n line
  | otherwise        = insertAt r (n - leftLines t) line

-- FIXME: Doesn't deal with corner cases
editAt :: BinaryTree EditorMeta Line -> Int -> Line -> BinaryTree EditorMeta Line
editAt (Value _ _) 1 line = makeValue line
editAt t@(BinaryTree m l r) n line
  | n <= leftLines t = BinaryTree (meta newl <> meta r) newl r
  | otherwise        = BinaryTree (meta l <> meta newr) l newr
  where
    newl = editAt l n line
    newr = editAt r (n - leftLines t) line

main :: IO ()
main = undefined
