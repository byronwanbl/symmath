module Simplify (simplify) where

import Control.Category ((>>>))
import Data.Function ((&))
import Lib (Expr (..))
import MaybeChanged (MaybeChanged (..), applyRecursively, applyRecursivelyUntilNoChanged, applyUntilNoChanged)
import Prelude

simplify :: Expr -> Expr
simplify =
  (counteractOppAndRec & applyRecursivelyUntilNoChanged)
    >>> (distributeMul & applyRecursivelyUntilNoChanged)
    >>> (reconstructAddAndMul & applyRecursivelyUntilNoChanged)

counteractOppAndRec :: Expr -> MaybeChanged Expr
counteractOppAndRec (Opp (Opp x)) = Changed x
counteractOppAndRec (Rec (Rec x)) = Changed x
counteractOppAndRec x = NoChanged x

distributeMul :: Expr -> MaybeChanged Expr
distributeMul (Mul (Add x y) z) = Changed (Add (Mul x z) (Mul y z))
distributeMul (Mul x (Add y z)) = Changed (Add (Mul x y) (Mul x z))
distributeMul (Opp (Add x y)) = Changed (Add (Opp x) (Opp y))
distributeMul (Rec (Mul x y)) = Changed (Mul (Rec x) (Rec y))
distributeMul x = NoChanged x

reconstructAddAndMul :: Expr -> MaybeChanged Expr
reconstructAddAndMul (Add x (Add y z)) = Changed (Add (Add x y) z)
reconstructAddAndMul (Mul x (Mul y z)) = Changed (Mul (Mul x y) z)
reconstructAddAndMul x = NoChanged x

expandPow :: Expr -> MaybeChanged Expr
expandPow (Pow x (Num 1)) = Changed x
expandPow (Pow x (Num i)) = Changed (Mul x (Pow x (Num (i - 1))))
expandPow x = NoChanged x
