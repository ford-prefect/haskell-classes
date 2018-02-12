module Parser where

import Control.Applicative
import Data.Char (digitToInt, isDigit )

newtype Parser i o = Parser { runParser :: i -> (Maybe o, i) }

instance Functor (Parser i) where
  fmap f p = Parser $ \input ->
    let (mo, i) = runParser p input
    in (f <$> mo, i)

instance Applicative (Parser i) where
  pure x = Parser $ \input -> (Just x, input)

  pf <*> po = Parser $ \input ->
    case runParser pf input of
         (Just f, rest) -> case runParser po rest of
                                (Just o, rest') -> (Just (f o), rest')
                                (Nothing, _)    -> (Nothing, input)
         (Nothing, _)   -> (Nothing, input)

instance Alternative (Parser i) where
  empty = Parser $ \input -> (Nothing, input)
  p1 <|> p2 = Parser $ \input ->
      case runParser p1 input of
           (Nothing, _) -> case runParser p2 input of
                                (Nothing, _) -> (Nothing, input)
                                justValue    -> justValue
           justValue    -> justValue

type Digit = Int

digit :: Parser String Digit
digit = Parser $ \input ->
  case input of
       (c:cs) | isDigit c -> (Just $ digitToInt c, cs)
       _                  -> (Nothing, input)

wholeNumber :: Parser String Int
wholeNumber = foldl (\num d -> num * 10 + d) 0 <$> many digit

--wholeNumber :: Parser String Int
--wholeNumber = Parser $ \input ->
--  case runParser digit input of
--       (Just d, rest) -> case runParser wholeNumber rest of
--                              (Just num, rest') -> (Just (d * (10 ^ numDigits num) + num), rest')
--                              (Nothing, rest')  -> (Just d, rest')
--       _              -> (Nothing, input)
--  where
--    numDigits = length . show

char :: Char -> Parser String Char
char x = Parser $ \input ->
  case input of
       (c:cs) | c == x -> (Just x, cs)
       _               -> (Nothing, input)

word :: String -> Parser String String
word "" = Parser $ \input -> (Just "", input)
word (c:cs) = (:) <$> char c <*> word cs

--word :: String -> Parser String String
--word "" = Parser $ \input -> (Just "", input)
--word (s:ss) = Parser $ \input ->
--  case runParser (char s) input of
--       (Just c, rest)  -> case runParser (word ss) rest of
--                               (Just w, rest') -> (Just (c:w), rest')
--                               (Nothing, rest) -> (Nothing, input)
--       (Nothing, rest) -> (Nothing, rest)
