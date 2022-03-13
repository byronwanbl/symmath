module MaybeChanged
  ( ApplyInto,
    MaybeChanged (..),
    apply,
    apply',
    chainBy,
    applyOnChanged,
    applyOnNoChanged,
    chain,
    peek,
    applyRecursively,
    applyUntilNoChanged,
  )
where

import Control.Category ((>>>))
import Data.Function ((&))
import Lib (Expr (..))
import Prelude

class ApplyInto a where
  -- applyInto f a : Apply f on a and a's sub elem
  applyInto :: (a -> MaybeChanged a) -> a -> MaybeChanged a

data MaybeChanged a = Changed a | NoChanged a

apply :: (a -> MaybeChanged a) -> MaybeChanged a -> MaybeChanged a
apply f x = chainBy const (f (peek x)) x

apply' :: (a -> a) -> MaybeChanged a -> MaybeChanged a
apply' f (Changed x) = Changed (f x)
apply' f (NoChanged x) = NoChanged (f x)

chainBy :: (a -> a -> a) -> MaybeChanged a -> MaybeChanged a -> MaybeChanged a
chainBy f (NoChanged x) (NoChanged y) = NoChanged (f x y)
chainBy f x y = Changed (f (peek x) (peek y))

applyOnChanged :: (a -> MaybeChanged a) -> MaybeChanged a -> MaybeChanged a
applyOnChanged f x@(Changed _) = apply f x
applyOnChanged _ a = a

applyOnNoChanged :: (a -> MaybeChanged a) -> MaybeChanged a -> MaybeChanged a
applyOnNoChanged f x@(NoChanged _) = apply f x
applyOnNoChanged _ a = a

chain :: MaybeChanged a -> MaybeChanged a -> MaybeChanged a
chain x y = chainBy const y x

peek :: MaybeChanged a -> a
peek (Changed x) = x
peek (NoChanged x) = x

applyRecursively :: ApplyInto a => (a -> MaybeChanged a) -> a -> MaybeChanged a
applyRecursively f x = apply (applyRecursively f & applyInto) (f x)

applyUntilNoChanged :: (a -> MaybeChanged a) -> a -> a
applyUntilNoChanged f x = case f x of
  Changed x' -> applyUntilNoChanged f x'
  NoChanged x' -> x'

instance ApplyInto Expr where
  applyInto f (Add x y) = chainBy Add (f x) (f y)
  applyInto f (Mul x y) = chainBy Mul (f x) (f y)
  applyInto f (Pow x y) = chainBy Pow (f x) (f y)
  applyInto f (Opp x) = apply' Opp (f x)
  applyInto f (Rec x) = apply' Rec (f x)
  applyInto _ x = NoChanged x

instance Show a => Show (MaybeChanged a) where
  show (Changed x) = "Changed(" <> show x <> ")"
  show (NoChanged x) = "NoChanged(" <> show x <> ")"
