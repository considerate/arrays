{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
module Data.Arrays.Dims where

import qualified Data.Arrays.Fin  as Fin
import           Data.Arrays.List
import qualified Data.Arrays.Pos  as Pos
import qualified Data.Arrays.Vec  as Vec
import           Data.Foldable    (toList)


type Dims dims = List Pos.Pos dims

class Known xs where
  known :: List Pos.Pos xs

instance Known '[] where
  known = Nil

instance (Pos.Known x, Known xs) => Known (x ': xs) where
  known = Cons Pos.known known

size :: List Pos.Pos dims -> Int
size = go 1
  where
  go :: Int -> List Pos.Pos dims -> Int
  go !acc Nil           = acc
  go !acc (Cons n dims) = go (acc * Pos.toInt n) dims

-- Assume little-endian order of dims
-- That is, the first index is the fastest changing and the
-- last is the slowest changing index.
index :: List Pos.Pos dims -> List Fin.Fin dims -> Int
index Nil Nil                   = 0
index (Cons l ls) (Cons i is) =
  Fin.toInt i + Pos.toInt l * index ls is

indices :: Pos.Pos n -> Vec.Vec n (Fin.Fin n)
indices Pos.One      = Vec.Cons Fin.Zero Vec.Nil
indices (Pos.Succ n) = Vec.Cons Fin.Zero (fmap Fin.Succ (indices n))

universe :: List Pos.Pos dims -> [List Fin.Fin dims]
universe Nil = [Nil]
universe (Cons n dims) = do
  is <- universe dims
  i <- toList (indices n)
  pure (Cons i is)
