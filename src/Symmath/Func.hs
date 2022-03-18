{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Symmath.Func (ApplyInto (..), Eq, Ord, Hashable) where

import Data.Hashable (Hashable (hashWithSalt))
import Symmath.Expr (Expr (..))
import Symmath.Utils.Others (exprKindId, unreachable)

class ApplyInto a b c where
  applyInto :: (a -> b) -> a -> c

instance Eq Expr where
  (==) (Add x1 y1) (Add x2 y2) = x1 == x2 && y1 == y2
  (==) (Mul x1 y1) (Mul x2 y2) = x1 == x2 && y1 == y2
  (==) (Pow x1 y1) (Pow x2 y2) = x1 == x2 && y1 == y2
  (==) (Opp x) (Opp y) = x == y
  (==) (Rec x) (Rec y) = x == y
  (==) (Alpha x) (Alpha y) = x == y
  (==) (Num x) (Num y) = x == y
  (==) x y = False

instance Ord Expr where
  compare (Add x1 y1) (Add x2 y2) = case compare x1 x2 of
    EQ -> compare y1 y2
    x -> x
  compare (Mul x1 y1) (Mul x2 y2) = case compare x1 x2 of
    EQ -> compare y1 y2
    x -> x
  compare (Pow x1 y1) (Pow x2 y2) = case compare x1 x2 of
    EQ -> compare y1 y2
    x -> x
  compare (Opp x) (Opp y) = compare x y
  compare (Rec x) (Rec y) = compare x y
  compare (Alpha x) (Alpha y) = compare x y
  compare (Num x) (Num y) = compare x y
  compare x y = case compare (exprKindId x) (exprKindId y) of
    EQ -> unreachable
    x -> x

instance Hashable Expr where
  hashWithSalt i (Add x y) = i + hashWithSalt i x + hashWithSalt i y
  hashWithSalt i (Mul x y) = i * 2 + hashWithSalt i x + hashWithSalt i y
  hashWithSalt i (Opp x) = i * 3 + hashWithSalt i x
  hashWithSalt i (Rec x) = i * 4 + hashWithSalt i x
  hashWithSalt i (Pow x y) = i * 5 + hashWithSalt i x + hashWithSalt i y
  hashWithSalt i (Alpha x) = hashWithSalt i x
  hashWithSalt i (Num x) = hashWithSalt i x