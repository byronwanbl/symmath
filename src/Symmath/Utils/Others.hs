module Symmath.Utils.Others
  ( show,
    priority,
    exprKindId,
    unreachable,
    (.+),
    (.-),
    (.*),
    (./),
    (.^),
    a,
    n,
  )
where

import Symmath.Expr (Expr (..))
import Prelude

instance Show Expr where
  show (Add x (Opp y)) = showWithBrackets 9 x <> " - " <> showWithBrackets 10 y
  show (Add x y) = showWithBrackets 9 x <> " + " <> showWithBrackets 10 y
  show (Opp x) = "-" <> showWithBrackets 10 x
  show (Mul x (Rec y)) = showWithBrackets 19 x <> " / " <> showWithBrackets 20 y
  show (Mul x y) = showWithBrackets 19 x <> " * " <> showWithBrackets 20 y
  show (Pow x y) = showWithBrackets 30 x <> " ^ " <> showWithBrackets 29 y
  show (Rec x) = "1 / " <> showWithBrackets 20 x
  show (Alpha c) = show c
  show (Num i) = show i

showWithBrackets :: Int -> Expr -> String
showWithBrackets pri expr
  | pri >= priority expr = "(" <> show expr <> ")"
  | otherwise = show expr

priority :: Expr -> Int
priority (Add _ _) = 10
priority (Opp _) = 10
priority (Mul _ _) = 20
priority (Rec _) = 20
priority (Pow _ _) = 30
priority (Alpha _) = 100
priority (Num _) = 100

exprKindId :: Expr -> Int
exprKindId (Add _ _) = 1
exprKindId (Opp _) = 2
exprKindId (Mul _ _) = 3
exprKindId (Rec _) = 4
exprKindId (Pow _ _) = 5
exprKindId (Alpha _) = 6
exprKindId (Num _) = 7

unreachable :: a
unreachable = error "Unreachable"

infixl 6 .+

(.+) = Add

infixl 6 .-

(.-) x y = Add x (Opp y)

infixl 7 .*

(.*) = Mul

infixl 7 ./

(./) x y = Mul x (Rec y)

infixr 8 .^

(.^) = Pow

a = Alpha

n = Num