module Strongweak.Generic.Weaken where

import Strongweak.Weaken

import GHC.Generics

weakenGeneric :: (Generic s, Generic w, GWeaken (Rep s) (Rep w)) => s -> w
weakenGeneric = to . gweaken . from

class GWeaken s w where
    gweaken :: s p -> w p

-- | Strip all meta.
instance GWeaken s w => GWeaken (M1 is ms s) (M1 iw mw w) where
    gweaken = M1 . gweaken . unM1

-- | Nothing to do for empty datatypes.
instance GWeaken V1 V1 where
    gweaken = id

-- | Nothing to do for empty constructors.
instance GWeaken U1 U1 where
    gweaken = id

-- | Special case: if source and target types are equal, copy the value through.
instance GWeaken (Rec0 s) (Rec0 s) where
    gweaken = id

-- | Weaken a field using the existing 'Weaken' instance.
instance {-# OVERLAPS #-} Weaken s w => GWeaken (Rec0 s) (Rec0 w) where
    gweaken = K1 . weaken . unK1

-- | Weaken product types by weakening left and right.
instance (GWeaken ls lw, GWeaken rs rw) => GWeaken (ls :*: rs) (lw :*: rw) where
    gweaken (l :*: r) = gweaken l :*: gweaken r

-- | Weaken sum types by weakening left or right.
instance (GWeaken ls lw, GWeaken rs rw) => GWeaken (ls :+: rs) (lw :+: rw) where
    gweaken = \case L1 l -> L1 $ gweaken l
                    R1 r -> R1 $ gweaken r
