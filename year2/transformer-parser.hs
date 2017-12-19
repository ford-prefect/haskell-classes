{-# Language GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}

module TransformerParseer where

import Control.Applicative

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f = Identity . f . runIdentity

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  (Identity a) >>= f = f a

class MonadState s m where
  get :: m s
  put :: s -> m ()

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f a = StateT $ \s -> pairf <$> runStateT a s
    where
      pairf (b, s') = (f b, s')

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  f <*> a = StateT $ \s -> do
    (f', s')  <- runStateT f s
    (a', s'') <- runStateT a s'
    return (f' a', s'')

instance (Monad m) => Monad (StateT s m) where
  a >>= f = StateT $ \s -> do
    (a', s') <- runStateT a s
    runStateT (f a') s'

instance (Applicative m) => MonadState s (StateT s m) where
  get = StateT $ \s -> pure (s, s)
  put s' = StateT $ \_ -> pure ((), s')

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = get >>= put . f

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty = StateT $ \_ -> empty
  sa <|> sb = StateT $ \s -> runStateT sa s <|> runStateT sb s

type State s a = StateT s Identity a

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  mf <*> ma = MaybeT $ pure (<*>) <*> runMaybeT mf <*> runMaybeT ma

instance (Monad m) => Monad (MaybeT m) where
  ma >>= f = MaybeT $ do
    a <- runMaybeT ma
    case a of
      Nothing -> return Nothing
      Just x  -> runMaybeT $ f x

instance (Applicative m) => Alternative (MaybeT m) where
  empty = MaybeT $ pure Nothing
  ma <|> mb = MaybeT $ pure (<|>) <*> runMaybeT ma <*> runMaybeT mb

-- write WriterT, and instances
-- write MonadWriter
-- implement MonadState in MonadWriter (delegate inwards)
-- implement MonadWriter in MonadState (delegate inwards)

newtype ParserT i m o = ParserT { runParserT :: StateT i (MaybeT m) o }
                        deriving (Functor, Applicative, Monad, Alternative)

type Parser i o = ParserT i Identity o

predParser :: (Monad m) => (a -> Bool) -> ParserT [a] m a
predParser p = ParserT $ do
  i <- get
  case i of
    (x : xs) | p x -> put xs >> return x
    _              -> empty

charParser :: (Monad m) => Char -> ParserT String m Char
charParser c = predParser (c == )

runParser :: Parser i o -> i -> Maybe (o, i)
runParser p = runIdentity . runMaybeT . runStateT (runParserT p)
