{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Strongweak.Weaken.GenericN where

import Strongweak.Weaken
import GHC.Generics
import GHC.TypeNats
import Unsafe.Coerce

weakenNGeneric
    :: forall (n :: Natural) a
    .  ( Generic (a n)
       , Generic (a (n+1))
       , GWeakenN n (Rep (a n)) (Rep (a (n+1)))
       ) => a n -> a (n+1)
weakenNGeneric = to . gweakenN @n . from

class GWeakenN n s w where
    gweakenN :: s p -> w p

-- | Strip all meta.
instance GWeakenN n s w => GWeakenN n (M1 is ms s) (M1 iw mw w) where
    gweakenN = M1 . gweakenN @n . unM1

-- | Nothing to do for empty datatypes.
instance GWeakenN n V1 V1 where
    gweakenN = id

-- | Nothing to do for empty constructors.
instance GWeakenN n U1 U1 where
    gweakenN = id

-- | Special case: if source and target types are equal, copy the value through.
--instance {-# OVERLAPPING #-} GWeaken (Rec0 s) (Rec0 s) where
--    gweaken = id

-- | Weaken a field using the existing 'Weaken' instance.
instance (WeakenedN n s ~ l, WeakenedN (n+1) w ~ r, Weaken s)
  => GWeakenN n (Rec0 l) (Rec0 r) where
    gweakenN = K1 . unsafeCoerce (weaken @s) . unK1

-- | Weaken product types by weakening left and right.
instance (GWeakenN n ls lw, GWeakenN n rs rw)
  => GWeakenN n (ls :*: rs) (lw :*: rw) where
    gweakenN (l :*: r) = gweakenN @n l :*: gweakenN @n r

-- | Weaken sum types by casing and weakening left or right.
instance (GWeakenN n ls lw, GWeakenN n rs rw)
  => GWeakenN n (ls :+: rs) (lw :+: rw) where
    gweakenN = \case L1 l -> L1 $ gweakenN @n l
                     R1 r -> R1 $ gweakenN @n r
