module Functor where

import Control.Applicative
import Data.Monoid ((<>))

newtype Identity a = Identity a deriving (Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


data Maybe' a = Just' a | Nothing' deriving (Show)

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
  pure = Just'
  Just' f <*> Just' v = Just' (f v)
  _       <*> _       = Nothing'

instance Alternative Maybe' where
  empty = Nothing'

  Just' j1 <|> _        = Just' j1
  _        <|> Just' j2 = Just' j2
  _        <|> _        = Nothing'



data Either' a b = Left' a | Right' b deriving (Show)

instance Functor (Either' a) where
  fmap _ (Left' l)  = Left' l
  fmap f (Right' r) = Right' (f r)

instance Applicative (Either' a) where
  pure = Right'
  Right' f <*> Right' v = Right' (f v)
  Left' e1 <*> _        = Left' e1
  _        <*> Left' e2 = Left' e2

instance Monoid a => Alternative (Either' a) where
  empty = Left' mempty

  Right' r1 <|> _         = Right' r1
  _         <|> Right' r2 = Right' r2
  Left' l1  <|> Left' l2  = Left' (l1 <> l2)


data List a = Cons a (List a) | Empty deriving (Show)

lappend :: List a -> List a -> List a
lappend l1 Empty = l1
lappend (Cons l1 l1s) l2 = Cons l1 (l1s `lappend` l2)

instance Functor List where
  fmap _ Empty       = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Empty
  (Cons f fs) <*> vs = fmap f vs `lappend` (fs <*> vs)
  _           <*> _  = Empty

instance Alternative List where
  empty = Empty
  (<|>) = lappend


data BST a = Node a (BST a) (BST a) | EmptyNode deriving (Show)

instance Functor BST where
  fmap _ EmptyNode    = EmptyNode
  fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)


data Pair a b = Pair a b deriving (Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)
