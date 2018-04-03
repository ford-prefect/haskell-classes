module Reader where

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader r r
ask = Reader id

instance Functor (Reader r) where
  fmap f a = Reader $ f . runReader a

instance Applicative (Reader r) where
  pure a = Reader $ const a
  (Reader ff) <*> (Reader fa) = Reader $ \r -> let f = ff r
                                                   a = fa r
                                               in f a

instance Monad (Reader r) where
  (Reader fa) >>= f = Reader $ \r -> let a = fa r
                                     in runReader (f a) r
