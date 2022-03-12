module Simplify where

import Prelude
import Data.Maybe (Maybe(..))
import Lib (Expr(..))

simplify :: Expr -> Expr
simplify e = e

reconstructAdd :: Expr -> Maybe Expr
reconstructAdd (Add x (Add y z)) = Just (Add (Add x y) z)

reconstructAdd _ = Nothing

reconstructMul :: Expr -> Maybe Expr
reconstructMul (Mul x (Mul y z)) = Just (Mul (Mul x y) z)

reconstructMul _ = Nothing

mulInto :: Expr -> Maybe Expr
mulInto (Mul (Add x y) z) = Just (Add (Mul x z) (Mul y z))

mulInto (Mul x (Add y z)) = Just (Add (Mul x y) (Mul x z))

mulInto _ = Nothing
