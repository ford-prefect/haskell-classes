module Functor where

newtype Identity a = Identity a deriving (Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


data Maybe' a = Just' a | Nothing' deriving (Show)

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)


data Either' a b = Left' a | Right' b deriving (Show)

instance Functor (Either' a) where
  fmap _ (Left' l)  = Left' l
  fmap f (Right' r) = Right' (f r)


data List a = Cons a (List a) | Empty deriving (Show)

instance Functor List where
  fmap _ Empty       = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


data BST a = Node a (BST a) (BST a) | EmptyNode deriving (Show)

instance Functor BST where
  fmap _ EmptyNode    = EmptyNode
  fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)


data Pair a b = Pair a b deriving (Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)
