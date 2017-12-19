import Data.Monoid
import Control.Applicative

data List a = Empty | Cons a (List a) deriving (Show)

fromList :: [a] -> List a
fromList = foldr Cons Empty

instance Monoid (List a) where
  mempty = Empty
  mappend Empty         l2    = l2
  mappend (Cons x1 xs1) l2    = Cons x1 (mappend xs1 l2)

instance Functor List where
  fmap f Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Empty
  Empty       <*> l = Empty
  (Cons f fs) <*> l = (f <$> l) <> (fs <*> l)
  --ZipList
  --(Cons f fs) <*> Empty = Empty
  --(Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

instance Alternative List where
  empty = Empty
  (<|>) = mappend

newtype AddableInt = AddableInt Int deriving (Show)

instance Monoid AddableInt where
  mempty                                = AddableInt 0
  mappend (AddableInt a) (AddableInt b) = AddableInt (a + b)

newtype MultiplyingInt = MultiplyingInt Int deriving (Show)

instance Monoid MultiplyingInt where
  mempty                                        = MultiplyingInt 1
  mappend (MultiplyingInt a) (MultiplyingInt b) = MultiplyingInt (a * b)

data MyMaybe a = MyNothing | MyJust a deriving (Show)

instance Monoid a => Monoid (MyMaybe a) where
  mempty = MyNothing

  mappend m1         MyNothing  = m1
  mappend MyNothing  m2         = m2
  mappend (MyJust a) (MyJust b) = MyJust $ a `mappend` b

instance Foldable MyMaybe where
  foldMap f MyNothing  = mempty
  foldMap f (MyJust a) = f a

instance Functor MyMaybe where
  fmap _ MyNothing  = MyNothing
  fmap f (MyJust a) = MyJust . f $ a

instance Applicative MyMaybe where
  pure = MyJust
  MyNothing <*> x = MyNothing
  MyJust f  <*> x = fmap f x

instance Alternative MyMaybe where
  empty = MyNothing
  MyNothing <|> m = m
  m         <|> _ = m

data MyFirst a = MyFirst (Maybe a) deriving Show

instance Monoid (MyFirst a) where
  mempty = MyFirst Nothing

  mappend x@(MyFirst v) y = case v of
    Nothing -> y
    _       -> x

data MyLast a = MyLast (Maybe a) deriving Show

instance Monoid (MyLast a) where
  mempty = MyLast Nothing

  mappend x y@(MyLast v) = case v of
    Nothing -> x
    _       -> y

instance Foldable List where
  foldMap f Empty       = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

-- foldr f z t = appEndo (foldMap (Endo . f) t) z

-- let f x l = x : l
-- foldr f [] $ fromList [1..5]
-- == (Endo . f) 1 <> (Endo . f) 2 <> (Endo . f) 3 <> ...
-- == (((((1 :) 2 :) 3 :) 4 :) 5 :) $ []
-- == 1 : (2 : (3 : (4 : (5 : []))))

data MyEither a b = MyLeft a | MyRight b deriving (Show)

instance Foldable (MyEither a) where
  foldMap f (MyLeft _)  = mempty
  foldMap f (MyRight b) = f b

instance Functor (MyEither a) where
  fmap f (MyLeft v)  = MyLeft v
  fmap f (MyRight x) = MyRight $ f x

instance Applicative (MyEither a) where
  pure = MyRight
  (MyLeft v)  <*> _           = MyLeft v
  (MyRight f) <*> (MyLeft v)  = MyLeft v
  (MyRight f) <*> (MyRight v) = MyRight (f v)

newtype Func a b = Func { getFunc :: a -> b }

instance Functor (Func a) where
  fmap f (Func g) = Func (f . g)

instance Applicative (Func a) where
  pure x = Func (\_ -> x)
  -- (x -> (a -> b)) -> (x -> a) -> (x -> b)
  (Func f) <*> (Func v) = Func (\x -> f x (v x))

main :: IO ()
main = undefined
