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
