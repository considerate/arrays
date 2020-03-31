{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Data.Arrays.List where

data List h xs where
  Nil :: List h '[]
  Cons :: h x -> List h xs -> List h (x ': xs)

instance Show (List h '[]) where
  show Nil = "Nil"

instance (Show (List h xs), Show (h x)) => Show (List h (x ': xs)) where
  showsPrec d (Cons x xs) =
    showParen (d >= 10)
    $ showString "Cons " . showsPrec 11 x . showString " " . showsPrec 11 xs
