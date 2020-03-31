{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Data.Arrays.Fin where

import qualified Data.Arrays.Nat as Nat

data Fin n where
  Zero :: Fin ('Nat.Succ n)
  Succ :: Fin n -> Fin ('Nat.Succ n)

toInt :: Fin n -> Int
toInt Zero     = 0
toInt (Succ n) = succ (toInt n)
