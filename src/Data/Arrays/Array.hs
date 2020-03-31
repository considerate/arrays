{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Arrays.Array where

import qualified Data.Arrays.Dims             as Dims
import qualified Data.Arrays.Fin              as Fin
import           Data.Arrays.List
import qualified Data.Arrays.Nat              as Nat
import qualified Data.Arrays.Pos              as Pos
import           Data.Foldable                (traverse_)
import qualified Data.Vector.Storable         as Vector
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Vector.Storable.Mutable as Mutable

showDims :: List Pos.Pos dims -> (Show (List Pos.Pos dims) => r) -> r
showDims Nil f         = f
showDims (Cons _ xs) f = showDims xs f

newtype V dims e = V (Vector.Vector e)
newtype D dims e = D (List Fin.Fin dims -> e)
newtype U dims e = U (Unboxed.Vector e)

instance (Show e, Vector.Storable e) => Show (V dims e) where
  show (V vec) = show vec

data Array dims e r where
  Array :: List Pos.Pos dims
    -> r dims e
    -> Array dims e r

instance (Show e, Vector.Storable e) => Show (Array dims e V) where
  showsPrec d (Array dims v)
    = showDims dims
    $ showParen (d >= 10)
    $ showString "Array " . showsPrec 10 dims . showString " " . showsPrec 10 v

instance (Dims.Known dims, Num e, Vector.Storable e, Num (r dims e)) => Num (Array dims e r) where
  fromInteger i = Array Dims.known (fromInteger i)
  Array dims xs + Array _ ys = Array dims (xs + ys)
  Array dims xs - Array _ ys = Array dims (xs - ys)
  Array dims xs * Array _ ys = Array dims (xs * ys)
  abs (Array dims xs) = Array dims (abs xs)
  signum (Array dims xs) = Array dims (signum xs)

fromFunction
  :: List Pos.Pos dims
  -> (List Fin.Fin dims -> e)
  -> Array dims e D
fromFunction dims f = Array dims (D f)

generate'
  :: forall dims e
  . Vector.Storable e
  => List Pos.Pos dims
  -> (List Fin.Fin dims -> e)
  -> Array dims e V
generate' dims f = Array dims $ V $ Vector.create $ do
  vec <- Mutable.unsafeNew (Dims.size dims)
  traverse_ (\ix -> Mutable.unsafeWrite vec (Dims.index dims ix) (f ix)) (Dims.universe dims)
  pure vec

generate
  :: (Vector.Storable e, Dims.Known dims)
  => (List Fin.Fin dims -> e)
  -> Array dims e V
generate = generate' Dims.known

fold :: Vector.Storable e
  => (e -> e -> e) -> e -> Array (d ': dims) e V -> Array dims e V
fold f z (Array (Cons d dims) (V xs))
  = generate' dims $ \ix ->
      Vector.foldl' f z $ Vector.map (\i ->
        xs Vector.! (i + Pos.toInt d * Dims.index dims ix)
      ) (Vector.enumFromTo 0 (pred $ Pos.toInt d))

sum :: (Vector.Storable e, Num e)
  => Array (d ': dims) e V -> Array dims e V
sum = fold (+) 0

backpermute
  :: Vector.Storable e
  => Dims.Dims dims'
  -> (List Fin.Fin dims' -> List Fin.Fin dims)
  -> Array dims e V
  -> Array dims' e V
backpermute dims' f (Array dims (V vec))
  = generate' dims' (\ix -> vec Vector.! Dims.index dims (f ix))

type Conv x k s p = 'Nat.Succ (Nat.Div ((x Nat.+ p) Nat.- k) s)

toNat' :: Pos.Pos n -> Nat.Nat' n
toNat' Pos.One = Nat.S Nat.Z
toNat' (Pos.Succ n) = Nat.S (toNat' n)

fromNat' :: Nat.Nat' ('Nat.Succ n) -> Pos.Pos ('Nat.Succ n)
fromNat' (Nat.S Nat.Z) = Pos.One
fromNat' (Nat.S m@Nat.S{}) = Pos.Succ (fromNat' m)

isPos :: Pos.Pos n -> (forall m. (n ~ 'Nat.Succ m) => r) -> r
isPos Pos.One = id
isPos Pos.Succ{} = id

data ConvDim x where
  ConvDim :: Pos.Pos k -> Pos.Pos s -> Nat.Nat' p -> ConvDim '(k,s,p)

convSize
  :: Pos.Pos k
  -> Pos.Pos s
  -> Nat.Nat' p
  -> Pos.Pos x
  -> Maybe (Pos.Pos (Conv x k s p))
convSize kernelSize stride padding x =
  case Nat.minus (Nat.add (toNat' x) padding) (toNat' kernelSize) of
    Nothing -> Nothing
    Just z -> Just
      $ isPos stride
      $ fromNat' (Nat.S (Nat.div' z (toNat' stride)))


type family Convolve convs dims filters :: [Nat.Nat] where
  Convolve '[] (channels ': dims) filters = (filters : dims)
  Convolve '[] '[] filters = '[filters]
  Convolve ('(k,s,p) ': convs) (d ': dims) filters =
    Conv d k s p ': Convolve convs dims filters

-- | Calcuate output dimensions after convolution
-- >>> let c = ConvDim Pos.One Pos.One Nat.Z
-- >>> convolveDims Pos.One (Cons c (Pos.Succ Pos.One) Nil) (Cons (Pos.Succ Pos.One) Nil)
-- Nothing
convolveDims
  ::  Pos.Pos filters
  -> List ConvDim convs
  -> List Pos.Pos dims
  -> Maybe (List Pos.Pos (Convolve convs dims filters))
convolveDims filters Nil (Cons _ dims) = Just $ Cons filters dims
convolveDims filters Nil Nil = Just $ Cons filters Nil
convolveDims _ (Cons _ _) Nil = Nothing
convolveDims filters (Cons (ConvDim k s p) convs) (Cons d dims) = do
  y <- convSize k s p d
  ys <- convolveDims filters convs dims
  pure (Cons y ys)
