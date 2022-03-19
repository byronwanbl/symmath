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
  ((calcKnown & applyRecursivelyL2R) >>> peek)
    >>> ((expandPow & applyRecursivelyR2L) >>> peek)
    >>> ((expandFraction & applyRecursivelyR2L) >>> peek)
    >>> (distributeMul & applyRecursivelyR2L & applyUntilNoChanged)
    >>> (reconstructAddAndMul & applyRecursivelyR2L & applyUntilNoChanged)
    >>> (reconstructAddAndMulByOrd & applyRecursivelyL2R & applyUntilNoChanged)
    >>> ((calcKnown & applyRecursivelyL2R) >>> peek)
    >>> ((counteractConst & applyRecursivelyR2L) >>> peek)
    >>> (mergeAdd & applyRecursivelyR2L & applyUntilNoChanged)
    >>> ((counteractConst & applyRecursivelyR2L) >>> peek)

distributeMul :: Expr -> MaybeChanged Expr
distributeMul (Mul (Add x y) z) = Changed (Add (Mul x z) (Mul y z))
distributeMul (Mul x (Add y z)) = Changed (Add (Mul x y) (Mul x z))
distributeMul x = NoChanged x

reconstructAddAndMul :: Expr -> MaybeChanged Expr
reconstructAddAndMul (Add x (Add y z)) = Changed (Add (Add x y) z)
reconstructAddAndMul (Mul x (Mul y z)) = Changed (Mul (Mul x y) z)
reconstructAddAndMul x = NoChanged x

expandPow :: Expr -> MaybeChanged Expr
expandPow (Pow x (Known 0)) = Changed (Known 0)
expandPow (Pow x (Known 1)) = Changed x
expandPow x@(Pow _ (Known (-1))) = NoChanged x
expandPow (Pow x (Known i))
  | i < 0 = Changed (Pow (Pow x (Known (- i))) (Known (-1)))
  | otherwise = Changed (Mul x (Pow x (Known (i - 1))))
expandPow x = NoChanged x

expandFraction :: Expr -> MaybeChanged Expr
expandFraction (Pow (Mul x y) (Known (-1))) = Changed (Mul (Pow x (Known (-1))) (Pow y (Known (-1))))
expandFraction x = NoChanged x

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
tryMergeAddR x y = tryMergeAdd x y

tryMergeAdd :: Expr -> Expr -> MaybeChanged Expr
tryMergeAdd x y =
  if x' == y'
    then
      maybe' Mul x' (Add (fromMaybe (Known 1) x'') (fromMaybe (Known 1) y'') & (calcKnown >>> peek))
        & ((reconstructAddAndMul & applyRecursivelyR2L) >>> peek)
        & Changed
    else NoChanged y
  where
    (x', x'') = splitKnown x
    (y', y'') = splitKnown y

    maybe' f x y = maybe y (f y) x

splitKnown :: Expr -> (Maybe Expr, Maybe Expr)
splitKnown (Mul x y) = (applyOnBoth Mul x' y', applyOnBoth Mul x'' y'')
  where
    (x', x'') = splitKnown x
    (y', y'') = splitKnown y
splitKnown x@(Known _) = (Nothing, Just x)
splitKnown x@(Unknown _) = (Just x, Nothing)
splitKnown x@(Pow _ _) = (Just x, Nothing)
splitKnown x@(Add _ _) = unreachable

counteractConst :: Expr -> MaybeChanged Expr
counteractConst (Add (Known 0) x) = Changed x
counteractConst (Add x (Known 0)) = Changed x
counteractConst (Mul (Known 0) _) = Changed (Known 0)
counteractConst (Mul _ (Known 0)) = Changed (Known 0)
counteractConst (Mul (Known 1) x) = Changed x
counteractConst (Mul x (Known 1)) = Changed x
counteractConst x = NoChanged x

calcKnown :: Expr -> MaybeChanged Expr
calcKnown (Add (Known x) (Known y)) = Changed (Known (x + y))
calcKnown (Mul (Known x) (Known y)) = Changed (Known (x * y))
calcKnown (Pow (Known x) (Known y)) = Changed (Known (x ^ y))
calcKnown x = NoChanged x
