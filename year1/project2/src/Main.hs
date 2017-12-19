module Main where

import Data.Monoid
import Control.Applicative
import Control.Monad

data Kuch a = KuchNahi | Bas a deriving (Show, Eq)

instance Monoid a => Monoid (Kuch a) where
  mempty                    = KuchNahi
  mappend KuchNahi KuchNahi = KuchNahi
  mappend (Bas a) KuchNahi  = Bas a
  mappend KuchNahi (Bas b)  = Bas b
  mappend (Bas a) (Bas b)   = Bas $ mappend a b

instance Functor Kuch where
  fmap _ KuchNahi = KuchNahi
  fmap f (Bas a)  = Bas $ f a

instance Applicative Kuch where
  pure                  = Bas
  KuchNahi <*> _        = KuchNahi
  _        <*> KuchNahi = KuchNahi
  (Bas f)  <*> (Bas a)  = Bas (f a)

instance Alternative Kuch where
  empty          = KuchNahi
  KuchNahi <|> a = a
  (Bas a)  <|> _ = Bas a

instance Monad Kuch where
  return         = pure
  fail _         = KuchNahi
  KuchNahi >>= _ = KuchNahi
  (Bas a) >>= f  = f a

instance MonadPlus Kuch where
  mzero = KuchNahi
  mplus = (<|>)

main :: IO ()
main = undefined
