{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Data.Arrays.Vec where

import qualified Data.Arrays.Nat as Nat

data Vec n a where
  Nil :: Vec 'Nat.Zero a
  Cons :: a -> Vec n a -> Vec ('Nat.Succ n) a

instance Functor (Vec n) where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable (Vec n) where
  foldMap _ Nil         = mempty
  foldMap f (Cons x as) = f x <> foldMap f as
