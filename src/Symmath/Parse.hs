module Symmath.Parse where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashSet (HashSet, empty, member, singleton, union)
import Symmath.Expr (Expr (Add, Alpha, Mul, Num, Opp, Pow, Rec))
import Symmath.Func ()
import Text.ParserCombinators.Parsec (Parser, char, digit, letter, many1, option, parse, spaces, try, (<|>))

type MetaData = HashSet Expr

sym :: Parser (Expr, MetaData)
sym =
  do
    d <- many1 digit
    return ((read d :: Integer) & Num, empty)
    <|> do
      l <- letter
      return (Alpha l, empty)
    <|> do
      char '('
      (e, m) <- rt
      char ')'
      return (e, singleton e)

pow :: Parser (Expr, MetaData)
pow =
  try
    ( do
        (x, m) <- sym
        spaces
        char '^'
        spaces
        (y, m') <- pow
        return (Pow x y, m `union` m')
    )
    <|> sym

mul :: Parser (Expr, MetaData)
mul =
  try
    ( do
        (x, m) <- pow
        spaces
        op <- char '*' <|> char '/'
        spaces
        (y, m') <- mul
        return ((if op == '*' then mul' else div') m' x y, m `union` m')
    )
    <|> pow
  where
    mul' m x t@(Mul y z)
      | member t m = Mul x t
      | otherwise = Mul (mul' m x y) z
    mul' m x y = Mul x y
    div' m x t@(Mul y z)
      | member t m = Mul x (Rec t)
      | otherwise = Mul (div' m x y) z
    div' m x y = Mul x (Rec y)

add :: Parser (Expr, MetaData)
add =
  try
    ( do
        (x, m) <- mul
        spaces
        op <- char '+' <|> char '-'
        spaces
        (y, m') <- add
        return ((if op == '+' then add' else sub') m' x y, m `union` m')
    )
    <|> mul
  where
    add' m x t@(Add y z)
      | member t m = Add x t
      | otherwise = Add (add' m x y) z
    add' m x y = Add x y
    sub' m x t@(Add y z)
      | member t m = Add x (Opp t)
      | otherwise = Add (sub' m x y) z
    sub' m x y = Add x (Opp y)

rt :: Parser (Expr, MetaData)
rt = add

parseExpr :: String -> Maybe Expr
parseExpr = parse rt "" >>> either (const Nothing) (fst >>> Just)