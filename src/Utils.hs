module Utils
  ( showExpr,
    priority,
  )
where

import Lib (Expr (..))
import Prelude

showExpr :: Expr -> String
showExpr (Add x (Opp y)) = showWithBrackets 10 x <> " - " <> showWithBrackets 10 y
showExpr (Add x y) = showWithBrackets 10 x <> " + " <> showWithBrackets 9 y
showExpr (Opp x) = "-" <> showWithBrackets 10 x
showExpr (Mul x (Rec y)) = showWithBrackets 20 x <> " / " <> showWithBrackets 20 y
showExpr (Mul x y) = showWithBrackets 20 x <> " * " <> showWithBrackets 19 y
showExpr (Pow x y) = showWithBrackets 30 x <> " ^ " <> showWithBrackets 30 y
showExpr (Rec x) = "1 / " <> showWithBrackets 20 x
showExpr (Alpha c) = show c
showExpr (Num i) = show i

showWithBrackets :: Int -> Expr -> String
showWithBrackets pri expr
  | pri >= priority expr = "(" <> showExpr expr <> ")"
  | otherwise = showExpr expr

priority :: Expr -> Int
priority (Add _ _) = 10
priority (Opp _) = 10
priority (Mul _ _) = 20
priority (Rec _) = 20
priority (Pow _ _) = 30
priority (Alpha _) = 100
priority (Num _) = 100
