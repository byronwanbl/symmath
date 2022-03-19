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
  (==) (Unknown x) (Unknown y) = x == y
  (==) (Known x) (Known y) = x == y
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
  compare (Unknown x) (Unknown y) = compare x y
  compare (Known x) (Known y) = compare x y
  compare x y = case compare (exprKindId x) (exprKindId y) of
    EQ -> unreachable
    x -> x

instance Hashable Expr where
  hashWithSalt i (Add x y) = i + hashWithSalt i x + hashWithSalt i y
  hashWithSalt i (Mul x y) = i * 2 + hashWithSalt i x + hashWithSalt i y
  hashWithSalt i (Pow x y) = i * 5 + hashWithSalt i x + hashWithSalt i y
  hashWithSalt i (Unknown x) = hashWithSalt i x
  hashWithSalt i (Known x) = hashWithSalt i x