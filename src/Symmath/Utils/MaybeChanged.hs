{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Symmath.Utils.MaybeChanged
  ( ApplyInto,
    MaybeChanged (..),
    apply,
    apply',
    tie,
    chainBy,
    applyOnChanged,
    applyOnNoChanged,
    chain,
    peek,
    applyRecursivelyR2L,
    applyRecursivelyL2R,
    applyUntilNoChanged,
  )
where

import Control.Category ((>>>))
import Data.Function ((&))
import Symmath.Expr (Expr (..))
import Symmath.Func (ApplyInto (..))
import Prelude

data MaybeChanged a = Changed a | NoChanged a

apply :: (a -> MaybeChanged a) -> MaybeChanged a -> MaybeChanged a
apply f (Changed x) = Changed (f x & peek)
apply f (NoChanged x) = f x

apply' :: (a -> a) -> MaybeChanged a -> MaybeChanged a
apply' f (Changed x) = Changed (f x)
apply' f (NoChanged x) = NoChanged (f x)

tie :: (a -> MaybeChanged a) -> (a -> MaybeChanged a) -> a -> MaybeChanged a
tie f g x = apply g (f x)

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

applyRecursivelyR2L :: ApplyInto a (MaybeChanged a) (MaybeChanged a) => (a -> MaybeChanged a) -> a -> MaybeChanged a
applyRecursivelyR2L f x = apply (applyRecursivelyR2L f & applyInto) (f x)

applyRecursivelyL2R :: ApplyInto a (MaybeChanged a) (MaybeChanged a) => (a -> MaybeChanged a) -> a -> MaybeChanged a
applyRecursivelyL2R f x = apply f (apply (applyRecursivelyL2R f & applyInto) (NoChanged x))

applyUntilNoChanged :: (a -> MaybeChanged a) -> a -> a
applyUntilNoChanged f x = case f x of
  Changed x' -> applyUntilNoChanged f x'
  NoChanged x' -> x'

instance ApplyInto Expr (MaybeChanged Expr) (MaybeChanged Expr) where
  applyInto f (Add x y) = chainBy Add (f x) (f y)
  applyInto f (Mul x y) = chainBy Mul (f x) (f y)
  applyInto f (Pow x y) = chainBy Pow (f x) (f y)
  applyInto _ x = NoChanged x

instance Show a => Show (MaybeChanged a) where
  show (Changed x) = "Changed " <> show x <> " "
  show (NoChanged x) = "NoChanged " <> show x <> " " 
