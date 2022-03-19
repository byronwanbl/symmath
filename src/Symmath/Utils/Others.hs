module Symmath.Utils.Others
  ( show,
    priority,
    exprKindId,
    applyOnBoth,
    unreachable,
    (.+),
    (.-),
    (.*),
    (./),
    (.^),
    u,
    k,
  )
where

import Symmath.Expr (Expr (..))
import Prelude

instance Show Expr where
  show (Add x y) = showWithBrackets 9 x <> " + " <> showWithBrackets 10 y
  show (Mul (Known (-1)) x) = "1 - " <> showWithBrackets 99 x
  show (Mul x y) = showWithBrackets 19 x <> " * " <> showWithBrackets 20 y
  show (Pow x y) = showWithBrackets 30 x <> " ^ " <> showWithBrackets 29 y
  show (Unknown c) = [c]
  show (Known i) = show i

showWithBrackets :: Int -> Expr -> String
showWithBrackets pri expr
  | pri >= priority expr = "(" <> show expr <> ")"
  | otherwise = show expr

priority :: Expr -> Int
priority (Add _ _) = 10
priority (Mul _ _) = 20
priority (Pow _ _) = 30
priority (Unknown _) = 100
priority (Known _) = 100

exprKindId :: Expr -> Int
exprKindId (Add _ _) = 6
exprKindId (Mul _ _) = 4
exprKindId (Pow _ _) = 3
exprKindId (Unknown _) = 2
exprKindId (Known _) = 1

applyOnBoth :: (u -> u -> u) -> Maybe u -> Maybe u -> Maybe u
applyOnBoth _ Nothing Nothing = Nothing
applyOnBoth _ x@(Just _) Nothing = x
applyOnBoth _ Nothing x@(Just _) = x
applyOnBoth f (Just x) (Just y) = Just (f x y)

unreachable :: u
unreachable = error "Unreachable"

infixl 6 .+

(.+) = Add

infixl 6 .-

(.-) x y = x .+ Known (-1) .* y

infixl 7 .*

(.*) = Mul

infixl 7 ./

(./) x y = x .* y .^ Known (-1)

infixr 8 .^

(.^) = Pow

u = Unknown

k = Known