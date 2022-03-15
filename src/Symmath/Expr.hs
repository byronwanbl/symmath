module Symmath.Expr (Expr(..)) where

data Expr
  = Add Expr Expr
  | Opp Expr
  | Mul Expr Expr
  | Rec Expr
  | Pow Expr Expr
  | Alpha Char
  | Num Integer 
