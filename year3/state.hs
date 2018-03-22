module State where

-- A State is a function from input state to output state and a value
newtype State s a = State { runState :: s -> (s, a) }

getState :: State s s
getState = State $ \s -> (s, s)

setState :: s -> State s ()
setState s = State . const $  (s, ())

instance Functor (State s) where
  fmap f fa = State $ \oldState -> let (newState, a) = runState fa oldState
                                   in (newState, f a)

instance Applicative (State s) where
  pure a = State $ \s -> (s, a)
  ff <*> fa = State $ \oldState -> let (newState, f)   = runState ff oldState
                                       (newerState, a) = runState fa newState
                                   in (newerState, f a)

instance Monad (State s) where
  fa >>= f = State $ \oldState -> let (newState, a) = runState fa oldState
                                  in runState (f a) newState

type Counter = Int

addTwo :: Int -> State Counter Int 
addTwo x = do
  count <- getState
  setState $ count + 1
  return $ x + 2

addTwo' x =
  getState                            -- State Counter Counter
  >>= \count -> setState (count + 1)  -- Counter -> State Counter ()
  >>  return (x + 2)                  -- State Counter Int
