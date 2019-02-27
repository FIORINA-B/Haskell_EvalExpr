{-# LANGUAGE InstanceSigs #-}
module Calc  where

import Control.Applicative

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Lit Double

eval :: Expr -> Double
eval e = case e of
  Add a b -> eval a + eval b
  Sub a b -> eval a - eval b
  Mul a b -> eval a * eval b
  Lit n   -> n

data Parser r = Parser {parse :: String -> Maybe (r, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Just (a, s') -> Just (f a, s')
    Nothing      -> Nothing

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  Parser p1 <*> pp2 = Parser $ \s -> case p1 s of
    Just (f, s') -> case parse pp2 s' of
      Just (a, s'') -> Just (f a, s'')
      Nothing       -> Nothing
    Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> pp2 = Parser $ \s -> p1 s <|> parse pp2 s

instance Monad Parser where
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser pA) f = Parser $ \s ->
    case pA s of
      Just (x, s') -> parse (f x) s'
      Nothing -> Nothing

runParser :: Parser a -> String -> Maybe a
runParser (Parser p) s = case p s of
  Just (r, "") -> Just r
  _            -> Nothing

check :: (Char -> Bool) -> Parser Char
check f = Parser $ \s -> case s of
  (x:xs) | f x -> Just (x, xs)
  _            -> Nothing

char :: Char -> Parser Char
char c = check (== c)

oneOf :: [Char] -> Parser Char
oneOf cs = check (\c -> elem c cs)

number :: Parser Double
number = pFloat <|> pInt
  where pFloat = read <$> (do
          intPart <- some digit
          _ <- char '.'
          fractionalPart <- some digit
          return (intPart ++ "." ++ fractionalPart))
        pInt = read <$> some digit
        digit = oneOf "0123456789"

expr :: Parser Expr
expr = add_sub
  where
    add_sub = binOp Add '+' mul <|> binOp Sub '-' mul <|> mul
    mul = binOp Mul '*' factor <|> factor
    factor = parens <|> lit
    lit = Lit <$> number
    parens = char '(' *> expr <* char ')'
    binOp c o p = c <$> p <*> (char o *> p)

evalExpr :: String -> Maybe Double
evalExpr s = (fmap eval) $ runParser expr $ concat $ words s
