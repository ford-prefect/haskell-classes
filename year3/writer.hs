module Writer where

newtype Writer o a = Writer { runWriter :: (o, a) }

tell :: o -> Writer o ()
tell o = Writer (o, ())

instance Functor (Writer o) where
  fmap f wa = let (o, a) = runWriter wa
              in Writer (o, f a) 

instance Monoid o => Applicative (Writer o) where
  pure a = Writer (mempty, a)
  wf <*> wa = let (o1, f) = runWriter wf
                  (o2, a) = runWriter wa
              in Writer (o1 `mappend` o2, f a)

instance Monoid o => Monad (Writer o) where
  wa >>= f = let (o1, a) = runWriter wa
                 (o2, b) = runWriter $ f a
             in Writer (o1 `mappend` o2, b)

f :: Int -> Writer [String] Int
f v = do
  tell ["going to start doing things"]
  tell ["really going to start doing things"]
  let rv = v + 1
  tell ["done doing things"]
  return rv
