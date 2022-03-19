module Symmath.Simplify where

import Control.Applicative (Applicative (liftA2))
import Control.Category ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe
import Symmath.Expr (Expr (..))
import Symmath.Utils.MaybeChanged
  ( MaybeChanged (..),
    apply',
    applyOnChanged,
    applyOnNoChanged,
    applyRecursivelyL2R,
    applyRecursivelyR2L,
    applyUntilNoChanged,
    chainBy,
    peek,
    tie,
  )
import Symmath.Utils.Others (applyOnBoth, unreachable)
import Prelude

simplify :: Expr -> Expr
simplify =
  ((numberCalc & applyRecursivelyL2R) >>> peek)
    >>> ((counteractOppAndRec & applyRecursivelyR2L) >>> peek)
    >>> ((expandPow & applyRecursivelyR2L) >>> peek)
    >>> ((distributeOppAndRec `tie` distributeMul) & applyRecursivelyR2L & applyUntilNoChanged)
    >>> (reconstructAddAndMul & applyRecursivelyR2L & applyUntilNoChanged)
    >>> (reconstructAddAndMulByOrd & applyRecursivelyL2R & applyUntilNoChanged)
    >>> ((numberCalc & applyRecursivelyL2R) >>> peek)
    >>> ((counteractConst & applyRecursivelyR2L) >>> peek)
    >>> (mergeAdd & applyRecursivelyR2L & applyUntilNoChanged)

counteractOppAndRec :: Expr -> MaybeChanged Expr
counteractOppAndRec (Opp (Opp x)) = Changed x
counteractOppAndRec (Rec (Rec x)) = Changed x
counteractOppAndRec x = NoChanged x

distributeMul :: Expr -> MaybeChanged Expr
distributeMul (Mul (Add x y) z) = Changed (Add (Mul x z) (Mul y z))
distributeMul (Mul x (Add y z)) = Changed (Add (Mul x y) (Mul x z))
distributeMul x = NoChanged x

distributeOppAndRec :: Expr -> MaybeChanged Expr
distributeOppAndRec (Opp (Add x y)) = Changed (Add (Opp x) (Opp y))
distributeOppAndRec (Rec (Mul x y)) = Changed (Mul (Rec x) (Rec y))
distributeOppAndRec x = NoChanged x

reconstructAddAndMul :: Expr -> MaybeChanged Expr
reconstructAddAndMul (Add x (Add y z)) = Changed (Add (Add x y) z)
reconstructAddAndMul (Mul x (Mul y z)) = Changed (Mul (Mul x y) z)
reconstructAddAndMul x = NoChanged x

expandPow :: Expr -> MaybeChanged Expr
expandPow (Pow x (Num 0)) = Changed (Num 0)
expandPow (Pow x (Num 1)) = Changed x
expandPow (Pow x (Num i))
  | i < 0 = error "Illegal pow"
  | otherwise = Changed (Mul x (Pow x (Num (i - 1))))
expandPow x = NoChanged x

reconstructAddAndMulByOrd :: Expr -> MaybeChanged Expr
reconstructAddAndMulByOrd a@(Add (Add x y) z) = if y > z then Changed (Add (Add x z) y) else NoChanged a
reconstructAddAndMulByOrd a@(Add x y) = if x > y then Changed (Add y x) else NoChanged a
reconstructAddAndMulByOrd a@(Mul (Mul x y) z) = if y > z then Changed (Mul (Mul x z) y) else NoChanged a
reconstructAddAndMulByOrd a@(Mul x y) = if x > y then Changed (Mul y x) else NoChanged a
reconstructAddAndMulByOrd x = NoChanged x

mergeAdd :: Expr -> MaybeChanged Expr
mergeAdd (Add x y) = tryMergeAddR y x & applyOnNoChanged (const (Add x y & NoChanged))
mergeAdd x = NoChanged x

tryMergeAddR :: Expr -> Expr -> MaybeChanged Expr
tryMergeAddR x (Add y z) =
  tryMergeAddR x y & applyOnChanged ((`Add` z) >>> NoChanged)
    & applyOnNoChanged (NoChanged >>> flip (chainBy Add) (tryMergeAdd x z))
tryMergeAddR x (Opp y) = tryMergeAddR x y & apply' Opp
tryMergeAddR x y = tryMergeAdd x y

tryMergeAdd :: Expr -> Expr -> MaybeChanged Expr
tryMergeAdd x y =
  if x' == y'
    then
      Changed
        ( maybe' Mul x' (Add (fromMaybe (Num 1) x'') (fromMaybe (Num 1) y'') & (numberCalc >>> peek))
            & removeMul1
            & ((reconstructAddAndMul & applyRecursivelyR2L) >>> peek)
        )
    else NoChanged y
  where
    (x', x'') = splitConst x
    (y', y'') = splitConst y

    removeMul1 (Mul (Num 1) x) = x
    removeMul1 x = x

    maybe' f x y = maybe y (f y) x

splitConst :: Expr -> (Maybe Expr, Maybe Expr)
splitConst (Opp x) = (x', x'' & fromMaybe (Num (-1)) & Just)
  where
    (x', x'') = splitConst x
splitConst (Mul x y) = (applyOnBoth Mul x' y', applyOnBoth Mul x'' y'' <&> (numberCalc >>> peek))
  where
    (x', x'') = splitConst x
    (y', y'') = splitConst y
splitConst x@(Num _) = (Nothing, Just x)
splitConst x@(Alpha _) = (Just x, Nothing)
splitConst x@(Pow _ _) = (Just x, Nothing)
splitConst x@(Rec _) = (Just x, Nothing)
splitConst x@(Add _ _) = unreachable

counteractConst :: Expr -> MaybeChanged Expr
counteractConst (Add (Num 0) x) = Changed x
counteractConst (Add x (Num 0)) = Changed x
counteractConst (Mul (Num 0) _) = Changed (Num 0)
counteractConst (Mul _ (Num 0)) = Changed (Num 0)
counteractConst (Mul (Num 1) x) = Changed x
counteractConst (Mul x (Num 1)) = Changed x
counteractConst (Mul (Opp x) (Opp y)) = Changed (Mul x y)
counteractConst (Mul (Opp x) y) = Changed (Opp (Mul x y))
counteractConst (Mul x (Opp y)) = Changed (Opp (Mul x y))
counteractConst x = NoChanged x

numberCalc :: Expr -> MaybeChanged Expr
numberCalc (Add (Num x) (Num y)) = Changed (Num (x + y))
numberCalc (Mul (Num x) (Num y)) = Changed (Num (x * y))
numberCalc (Pow (Num x) (Num y)) = Changed (Num (x ^ y))
numberCalc (Rec (Num x)) = Changed (Num (- x))
numberCalc (Opp (Num x)) = Changed (Num (1 `div` x))
numberCalc x = NoChanged x
