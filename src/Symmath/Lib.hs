module Symmath.Lib
  ( Expr (..),
    parseExpr,
    show,
    simplify,
    Eq,
    Hashable,
    Ord,
    a,
    n,
    (.*),
    (.+),
    (.-),
    (./),
    (.^),
  )where

import Symmath.Expr (Expr (..))
import Symmath.Func (Eq, Hashable, Ord)
import Symmath.Parse (parseExpr)
import Symmath.Simplify (simplify)
import Symmath.Utils.Others
  ( a,
    n,
    show,
    (.*),
    (.+),
    (.-),
    (./),
    (.^),
  )
