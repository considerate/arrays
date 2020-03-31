{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Arrays.Pos where

import qualified Data.Arrays.Nat as Nat

data Pos n where
  One :: Pos ('Nat.Succ 'Nat.Zero)
  Succ :: Pos n -> Pos ('Nat.Succ n)

instance Show (Pos n) where
  show p = show (toInt p)

toInt :: Pos n -> Int
toInt = go 1
  where
  go :: Int -> Pos n -> Int
  go !acc One      = acc
  go !acc (Succ n) = go (succ acc) n

class Known x where
  known :: Pos x

instance Known ('Nat.Succ 'Nat.Zero) where
  known = One

instance (Known ('Nat.Succ x)) => Known ('Nat.Succ ('Nat.Succ x)) where
  known = Succ known
