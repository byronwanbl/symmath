{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Symmath.Func (ApplyInto (..)) where

import Symmath.Expr (Expr (..))
import Symmath.Utils.Others (exprKindId, unwrapPair, unreachable)

class ApplyInto a b c where
  applyInto :: (a -> b) -> a -> c

instance Eq Expr where
  (==) (Alpha x) (Alpha y) = x == y
  (==) (Num x) (Num y) = x == y
  (==) x y = (exprKindId x == exprKindId y) && applyInto (unwrapPair (==)) (x, y)

instance ApplyInto (Expr, Expr) Bool Bool where
  applyInto f (Add x1 y1, Add x2 y2) = f (x1, x2) && f (y1, y2)
  applyInto f (Mul x1 y1, Mul x2 y2) = f (x1, x2) && f (y1, y2)
  applyInto f (Pow x1 y1, Pow x2 y2) = f (x1, x2) && f (y1, y2)
  applyInto f (Opp x1, Opp x2) = f (x1, x2)
  applyInto f (Rec x1, Rec x2) = f (x1, x2)
  applyInto _ _ = unreachable

instance Ord Expr where
  compare (Alpha x) (Alpha y) = compare x y
  compare (Num x) (Num y) = compare x y
  compare x y = case compare (exprKindId x) (exprKindId y) of
    LT -> LT
    GT -> GT
    EQ -> applyInto (unwrapPair compare) (x, y)

instance ApplyInto (Expr, Expr) Ordering Ordering where
  applyInto f (Add x1 y1, Add x2 y2) = case f (x1, x2) of
    LT -> LT
    GT -> GT
    EQ -> f (y1, y2)
  applyInto f (Mul x1 y1, Mul x2 y2) = case f (x1, x2) of
    LT -> LT
    GT -> GT
    EQ -> f (y1, y2)
  applyInto f (Pow x1 y1, Pow x2 y2) = case f (x1, x2) of
    LT -> LT
    GT -> GT
    EQ -> f (y1, y2)
  applyInto f (Opp x1, Opp x2) = f (x1, x2)
  applyInto f (Rec x1, Rec x2) = f (x1, x2)
  applyInto _ _ = unreachable