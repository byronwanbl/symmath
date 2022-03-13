module Simplify where

import Data.Function ((&))
import Lib (Expr (..))
import MaybeChanged (MaybeChanged (..), applyRecursively, applyUntilNoChanged)
import Prelude
import Control.Category ((>>>))

simplify :: Expr -> Expr
simplify = mulIntoR >>> reconstructMulR >>> reconstructAddR

reconstructAdd :: Expr -> MaybeChanged Expr
reconstructAdd (Add x (Add y z)) = Changed (Add (Add x y) z)
reconstructAdd x = NoChanged x

reconstructMul :: Expr -> MaybeChanged Expr
reconstructMul (Mul x (Mul y z)) = Changed (Mul (Mul x y) z)
reconstructMul x = NoChanged x

mulInto :: Expr -> MaybeChanged Expr
mulInto (Mul (Add x y) z) = Changed (Add (Mul x z) (Mul y z))
mulInto (Mul x (Add y z)) = Changed (Add (Mul x y) (Mul x z))
mulInto x = NoChanged x

reconstructAddR :: Expr -> Expr
reconstructAddR = (reconstructAdd & applyRecursively) & applyUntilNoChanged

reconstructMulR :: Expr -> Expr
reconstructMulR = (reconstructMul & applyRecursively) & applyUntilNoChanged

mulIntoR :: Expr -> Expr
mulIntoR = (mulInto & applyRecursively) & applyUntilNoChanged