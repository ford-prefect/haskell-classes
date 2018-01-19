module Monoid where

import Data.Monoid ((<>))

newtype Prod = Prod Int deriving (Show)

instance Monoid Prod where
  mempty = Prod 1
  mappend (Prod x) (Prod y) = Prod (x * y)

data Mebbe a = Nada | Vada a

instance Show a => Show (Mebbe a) where
  show Nada     = "Nada"
  show (Vada a) = "Yada " ++ show a

instance Monoid a => Monoid (Mebbe a) where
  mempty                    = Nada
  mappend x Nada            = x
  mappend Nada x            = x
  mappend (Vada x) (Vada y) = Vada (x <> y)


newtype First a = First (Maybe a) deriving (Show)

instance Monoid (First a) where
  mempty = First Nothing
  mappend x@(First (Just _)) _ = x
  mappend (First Nothing) x    = x

newtype Last a = Last (Maybe a) deriving (Show)

instance Monoid (Last a) where
  mempty = Last Nothing
  mappend _ x@(Last (Just _)) = x
  mappend x (Last Nothing)    = x


newtype All = All Bool deriving (Show)

instance Monoid All where
  mempty = All True
  mappend (All False) _         = All False
  mappend _ (All False)         = All False
  mappend (All True) (All True) = All True

newtype Any = Any Bool deriving (Show)

instance Monoid Any where
  mempty = Any False
  mappend (Any True) _            = Any True
  mappend _ (Any True)            = Any True
  mappend (Any False) (Any False) = Any False


newtype Min a = Min a deriving (Show)

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound
  mappend (Min x) (Min y) = Min $ min x y

newtype Max a = Max a deriving (Show)

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max maxBound
  mappend (Max x) (Max y) = Max $ max x y


data List a = Cons a (List a) | Empty deriving (Show)

instance Monoid (List a) where
  mempty = Empty
  mappend Empty b       = b
  mappend a Empty       = a
  mappend (Cons a as) b = Cons a (as <> b)


data BST a = Node a (BST a) (BST a) | EmptyNode deriving (Show)

insert :: Ord a => BST a -> a -> BST a
insert EmptyNode x = Node x EmptyNode EmptyNode
insert (Node v l r) x
  | x < v     = Node v (insert l x) r
  | x > v     = Node v l (insert r x)
  | otherwise = error "Tried to insert duplicate"

instance Ord a => Monoid (BST a) where
  mempty = EmptyNode

  mappend EmptyNode b    = b
  mappend a EmptyNode    = a
  mappend (Node v l r) b = l <> insert b v <> r
