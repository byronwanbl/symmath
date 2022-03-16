{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Symmath.Func (ApplyInto (..)) where

import Symmath.Expr (Expr (..))
import Symmath.Utils.Others (exprKindId, unreachable)

class ApplyInto a b c where
  applyInto :: (a -> b) -> a -> c

newtype ExprPairEq = ExprPairEq (Expr, Expr)

instance Eq Expr where
  (==) (Alpha x) (Alpha y) = x == y
  (==) (Num x) (Num y) = x == y
  (==) x y = (exprKindId x == exprKindId y) && applyInto (\(ExprPairEq (x, y)) -> x == y) (ExprPairEq (x, y))

instance ApplyInto ExprPairEq Bool Bool where
  applyInto f (ExprPairEq (Add x1 y1, Add x2 y2)) = f (ExprPairEq (x1, x2)) && f (ExprPairEq (y1, y2))
  applyInto f (ExprPairEq (Mul x1 y1, Mul x2 y2)) = f (ExprPairEq (x1, x2)) && f (ExprPairEq (y1, y2))
  applyInto f (ExprPairEq (Pow x1 y1, Pow x2 y2)) = f (ExprPairEq (x1, x2)) && f (ExprPairEq (y1, y2))
  applyInto f (ExprPairEq (Opp x1, Opp x2)) = f (ExprPairEq (x1, x2))
  applyInto f (ExprPairEq (Rec x1, Rec x2)) = f (ExprPairEq (x1, x2))
  applyInto _ _ = unreachable

instance Ord Expr where
  compare (Alpha x) (Alpha y) = compare x y
  compare (Num x) (Num y) = compare x y
  compare x y = case compare (exprKindId x) (exprKindId y) of
    LT -> LT
    GT -> GT
    EQ -> applyInto (\(ExprPairOrd (x, y)) -> compare x y) (ExprPairOrd (x, y))

newtype ExprPairOrd = ExprPairOrd (Expr, Expr)

instance ApplyInto ExprPairOrd Ordering Ordering where
  applyInto f (ExprPairOrd (Add x1 y1, Add x2 y2)) = case f (ExprPairOrd (x1, x2)) of
    LT -> LT
    GT -> GT
    EQ -> f (ExprPairOrd (y1, y2))
  applyInto f (ExprPairOrd (Mul x1 y1, Mul x2 y2)) = case f (ExprPairOrd (x1, x2)) of
    LT -> LT
    GT -> GT
    EQ -> f (ExprPairOrd (y1, y2))
  applyInto f (ExprPairOrd (Pow x1 y1, Pow x2 y2)) = case f (ExprPairOrd (x1, x2)) of
    LT -> LT
    GT -> GT
    EQ -> f (ExprPairOrd (y1, y2))
  applyInto f (ExprPairOrd (Opp x1, Opp x2)) = f (ExprPairOrd (x1, x2))
  applyInto f (ExprPairOrd (Rec x1, Rec x2)) = f (ExprPairOrd (x1, x2))
  applyInto _ _ = unreachable
