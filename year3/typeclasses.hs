module TypeClasses where

class Show' a where
  show' :: a -> String

data Colour = Red | Green | Blue

instance Show' Colour where
  show' x = case x of
    Red   -> "R"
    Green -> "G"
    Blue  -> "B"

data Maybe' a = Just' a | Nothing'

instance (Show' a) => Show' (Maybe' a) where
  show' Nothing' = "Nothing'"
  show' (Just' a) = "Just " ++ show' a

instance Show' Bool where
  show' True  = "T"
  show' False = "F"

data List a = Empty | Cons a (List a) -- deriving Show

instance Show a => Show (List a) where
  show l = "{" ++ showPrime l ++ "}"
    where
      showPrime Empty          = " "
      showPrime (Cons x Empty) = show x
      showPrime (Cons x xs)    = show x ++ ", " ++ showPrime xs

class Eq' a where
  equal :: a -> a -> Bool
  equal x y = not (notEqual x y)

  notEqual :: a -> a -> Bool
  notEqual x y = not (equal x y)

  {-# MINIMAL equal | notEqual #-}

instance Eq' Colour where
  equal Red Red     = True
  equal Green Green = True
  equal Blue Blue   = True
  equal _ _         = False

instance Eq' a => Eq' (List a) where
  equal Empty Empty             = True
  equal (Cons x xs) (Cons y ys) = x `equal` y && xs `equal` ys
  equal _ _                     = False

  notEqual Empty Empty             = False
  notEqual (Cons x xs) (Cons y ys) = x `notEqual` y || xs `notEqual` ys
  notEqual _ _                     = True

data Ordering' = LessThan | Equal | GreaterThan

data Direction = N | E | S | W deriving Show

instance Enum Direction where
  fromEnum N = 0
  fromEnum E = 1
  fromEnum S = 2
  fromEnum W = 3

  toEnum n =
    case n `mod` 4 of
         0 -> N
         1 -> E
         2 -> S
         3 -> W
