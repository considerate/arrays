{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Arrays.Nat where


import           Prelude (Maybe (..), Ordering (..), Show, fst, snd)

-- $setup
-- >>> import Prelude(($))

type N0 = 'Zero
type N1 = 'Succ N0
type N2 = 'Succ N1
type N3 = 'Succ N2
type N4 = 'Succ N3
type N5 = 'Succ N4
type N6 = 'Succ N5
type N7 = 'Succ N6
type N8 = 'Succ N7
type N9 = 'Succ N8

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Nat' n where
  Z :: Nat' 'Zero
  S :: Nat' n -> Nat' ('Succ n)
deriving instance Show (Nat' n)


type family n + m where
  'Zero + m = m
  'Succ n + m = 'Succ (n + m)

add :: Nat' n -> Nat' m -> Nat' (n + m)
add Z m     = m
add (S n) m = S (add n m)

type family n :* m where
  'Zero :* m = 'Zero
  'Succ n :* m = m + (n :* m)

times :: Nat' n -> Nat' m -> Nat' (n :* m)
times Z _     = Z
times (S n) m = add m (times n m)

type family n - m where
  n - 'Zero = n
  'Succ n - 'Succ m = n - m

-- |
-- >>> minus (S $ S Z) (S Z)
-- Just (S Z)
-- >>> minus (S $ S $ S Z) (S Z)
-- Just (S (S Z))
-- >>> minus (S Z) (S (S Z))
-- Nothing
minus :: Nat' n -> Nat' m -> Maybe (Nat' (n - m))
minus n Z         = Just n
minus Z (S _)     = Nothing
minus (S n) (S m) = minus n m

pred :: Nat' ('Succ n) -> Nat' n
pred (S n) = n


safeMinus :: LEQ m n -> Nat' n -> Nat' m -> Nat' (n - m)
safeMinus LEQEQ Z Z         = Z
safeMinus LEQEQ (S n) (S m) = safeMinus LEQEQ n m
safeMinus LEQLT n Z         = n
safeMinus LEQLT (S n) (S m) = safeMinus LEQLT n m

type DivMod n m = '(Div n m, Mod n m)
type family Div n m where
  Div n ('Succ m) = Div' n ('Succ m) 'Zero n (Cmp n ('Succ m))

type family Mod n m where
  Mod n ('Succ m) = Mod' n ('Succ m) 'Zero n (Cmp n ('Succ m))

-- | SLOW euclidean division algorithm
type family Div' n d q r cmp where
  Div' n ('Succ d) q r 'LT = q
  Div' n ('Succ d) q r 'EQ =
    Div' n ('Succ d) ('Succ q) (r - 'Succ d) (Cmp (r - 'Succ d) ('Succ d))
  Div' n ('Succ d) q r 'GT =
    Div' n ('Succ d) ('Succ q) (r - 'Succ d) (Cmp (r - 'Succ d) ('Succ d))

type family Mod' n d q r cmp where
  Mod' n ('Succ d) q r 'LT = r
  Mod' n ('Succ d) q r 'EQ =
    Mod' n ('Succ d) ('Succ q) (r - 'Succ d) (Cmp (r - 'Succ d) ('Succ d))
  Mod' n ('Succ d) q r 'GT =
    Mod' n ('Succ d) ('Succ q) (r - 'Succ d) (Cmp (r - 'Succ d) ('Succ d))


-- |
-- >>> div (S (S Z)) (S Z)
-- Just (S (S Z))
-- >>> div Z Z
-- Nothing
-- >>> div Z (S (S Z))
-- Just Z
-- >>> div (S $ S $ S $ S $ S $ S Z) (S $ S Z)
-- Just (S (S (S Z)))
div :: Nat' n -> Nat' m -> Maybe (Nat' (Div n m))
div _ Z     = Nothing
div n (S m) = Just (div' n (S m))

div' :: Nat' n -> Nat' ('Succ m) -> Nat' (Div n ('Succ m))
div' n m = fst (divMod n m)

mod :: Nat' n -> Nat' m -> Maybe (Nat' (Mod n m))
mod _ Z     = Nothing
mod n (S m) = Just (mod' n (S m))

mod' :: Nat' n -> Nat' ('Succ m) -> Nat' (Mod n ('Succ m))
mod' n m = snd (divMod n m)

divMod
  :: Nat' n
  -> Nat' ('Succ m)
  -> ( Nat' (Div n ('Succ m))
     , Nat' (Mod n ('Succ m))
     )
divMod n d = divMod' n d Z n (compare n d)

divMod'
  :: Nat' n
  -> Nat' ('Succ d)
  -> Nat' q
  -> Nat' r
  -> Comparison r ('Succ d)
  -> ( Nat' (Div' n ('Succ d) q r (Cmp r ('Succ d)))
     , Nat' (Mod' n ('Succ d) q r (Cmp r ('Succ d)))
     )
divMod' _ _ q r CmpLT = (q,r)
divMod' n d q r CmpEQ =
  let r' = safeMinus LEQEQ r d
  in divMod' n d (S q) r' (compare r' d)
divMod' n d q r CmpGT =
  let r' = safeMinus LEQLT r d
  in divMod' n d (S q) r' (compare r' d)

type family Cmp (a :: k) (b :: k) :: Ordering

type instance Cmp 'Zero 'Zero = 'EQ
type instance Cmp 'Zero ('Succ m) = 'LT
type instance Cmp ('Succ n) 'Zero = 'GT
type instance Cmp ('Succ n) ('Succ m) = Cmp n m

data LEQ a b where
  LEQLT :: Cmp a b ~ 'LT => LEQ a b
  LEQEQ :: Cmp a b ~ 'EQ => LEQ a b

type family CaseCmp (o :: Ordering) (a :: k) (b :: k) (c :: k) where
  CaseCmp 'LT a b c = a
  CaseCmp 'EQ a b c = b
  CaseCmp 'GT a b c = c

data Comparison a b where
  CmpLT :: (Cmp a b ~ 'LT, Cmp b a ~ 'GT) => Comparison a b
  CmpEQ :: (Cmp a b ~ 'EQ, Cmp b a ~ 'EQ) => Comparison a b
  CmpGT :: (Cmp a b ~ 'GT, Cmp b a ~ 'LT) => Comparison a b

compare :: Nat' n -> Nat' m -> Comparison n m
compare Z Z         = CmpEQ
compare Z (S _)     = CmpLT
compare (S _) Z     = CmpGT
compare (S n) (S m) =
  case compare n m of
    CmpLT -> CmpLT
    CmpEQ -> CmpEQ
    CmpGT -> CmpGT
