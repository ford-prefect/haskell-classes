module Monad where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe

import Debug.Trace

data HoSaktaHai a = NahiHai | YehLo a deriving (Show)

instance Functor HoSaktaHai where
  fmap f NahiHai   = NahiHai
  fmap f (YehLo a) = YehLo $ f a

instance Applicative HoSaktaHai where
  pure   = YehLo
  NahiHai <*> _       = NahiHai
  _       <*> NahiHai = NahiHai
  YehLo f <*> YehLo a = YehLo $ f a

instance Alternative HoSaktaHai where
  empty         = NahiHai
  NahiHai <|> a = a
  a       <|> _ = a

instance Monad HoSaktaHai where
  NahiHai >>= _ = NahiHai
  YehLo a >>= f = f a


data Suchee a = Khalee | Jod a (Suchee a) deriving (Show)

instance Functor Suchee where
  fmap f Khalee = Khalee
  fmap f (Jod x xs) = Jod (f x) (fmap f xs)

instance Monoid (Suchee a) where
  mempty = Khalee
  mappend Khalee a = a
  mappend a Khalee = a
  mappend (Jod a as) b = Jod a (as `mappend` b)

instance Applicative Suchee where
  pure a = Jod a Khalee
  Khalee   <*> _      = Khalee
  _        <*> Khalee = Khalee
  Jod f fs <*> xs     = (f <$> xs) `mappend` (fs <*> xs)

instance Alternative Suchee where
  empty        = Khalee
  Khalee <|> a = a
  a      <|> _ = a
  
instance Monad Suchee where
  Khalee   >>= _ = Khalee
  Jod a as >>= f = f a `mappend` (as >>= f)

newtype Reader e a = Reader { runReader :: (e -> a) }

type UserId = Int
type UserAge = Int
type UserName = String

data User = User { userName :: UserName, userAge :: UserAge } deriving (Show)
type Users = [(UserId, User)]

data Env = Env { users :: Users, primeUsers :: [UserId] } deriving (Show)

env :: Env
env = Env { users      = [(1, User "abhinav" 44), (2, User "arun" 22), (3, User "ashish" 11)]
          , primeUsers = [1]
      }

getUser :: Env -> UserId -> Maybe User
getUser env id = lookup id (users env)

getUserName :: Env -> UserId -> Maybe UserName
getUserName env id = userName <$> getUser env id

getUserAge :: Env -> UserId -> Maybe UserAge
getUserAge env id = userAge <$> getUser env id

getPrimeUsers :: Env -> [User]
getPrimeUsers env = Data.Maybe.mapMaybe (getUser env) (primeUsers env)

getUser' :: UserId -> Reader Env (Maybe User)
getUser' id = undefined

newtype Writer w a = Writer { runWriter :: (a, w) }

tell :: (Monoid w) => w -> Writer w ()
tell s = Writer ((), s)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap ff sf = State $ \s -> let (a, s') = runState sf s
                             in (ff a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  sf <*> sa = State $ \s -> let (f, s') = runState sf s
                                (a, s'') = runState sa s'
                            in (f a, s'')

instance Monad (State s) where
  return = pure
  sa >>= f = State $ \s -> let (a, s') = runState sa s
                           in runState (f a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s' = State $ \_ -> ((), s')

modify :: (Show s) => (s -> s) -> State s ()
modify f = do
  s <- get
  put . f $ s

upCase :: String -> State Int String
upCase str = do
  count <- get
  put (count + 1)
  return $ map toUpper str
