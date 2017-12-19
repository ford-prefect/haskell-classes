{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import System.Environment (getArgs)
import Text.Parsec hiding ((<|>))

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Lit Integer deriving ( Eq, Show )

spaceChar :: Stream s m Char => Char -> ParsecT s u m ()
spaceChar c = spaces *> char c *> spaces

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

literal :: Parsec String () Expr
literal = Lit . read <$> (spaces *> many1 digit <* spaces)

parens :: Parsec String () Expr
parens = between (spaceChar '(') (spaceChar ')') expr

simpleExpr :: Parsec String () Expr
simpleExpr = try literal <|> try parens

add :: Parsec String () Expr
add = Add <$> simpleExpr <*> (char '+' *> expr)

mul :: Parsec String () Expr
mul = Mul <$> simpleExpr <*> (char '*' *> expr)

expr :: Parsec String () Expr
expr = try parens
   <|> try mul
   <|> try add
   <|> try literal

main :: IO ()
main = do
  args <- getArgs
  print args
