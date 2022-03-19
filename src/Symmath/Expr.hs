module Symmath.Expr (Expr(..)) where

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Pow Expr Expr
  | Unknown Char
  | Known Integer 
