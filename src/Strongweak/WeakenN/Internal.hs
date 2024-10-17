{-# LANGUAGE UndecidableInstances #-} -- type family 'Weakened' in constraints
{-# LANGUAGE AllowAmbiguousTypes #-} -- ambiguous intermediate type classes

{- | 'Strongweak.WeakenN.WeakenN' internals.

Just in case. You shouldn't need these, but they might be fun to look at.

__Internal module. Exports may change without warning. Try not to use.__
-}

module Strongweak.WeakenN.Internal where

import Strongweak.Weaken ( Weaken(weaken), type WeakenedN )
import Strongweak.Strengthen
import GHC.TypeNats ( type Natural, type (-) )
import Unsafe.Coerce ( unsafeCoerce )

class WeakenWeakenN (n :: Natural) a where
    weakenWeakenN :: a -> WeakenedN n a

-- | Zero case: return the value as-is.
instance {-# OVERLAPPING #-} WeakenWeakenN 0 a where
    weakenWeakenN = id

-- | Inductive case. @n /= 0@, else this explodes.
instance (Weaken a, WeakenWeakenN (n-1) (Weakened a))
  => WeakenWeakenN n a where
    weakenWeakenN a =
        case weakenWeakenN @(n-1) @(Weakened a) (weaken a) of
          x -> weakenedNRL1 @n @a x

-- | Inverted inductive 'WeakenedN'case.
--
-- @n@ must not be 0.
weakenedNRL1 :: forall n a. WeakenedN (n-1) (Weakened a) -> WeakenedN n a
weakenedNRL1 = unsafeCoerce

class WeakenWeakenN n a => StrengthenWeakenN (n :: Natural) a where
    strengthenWeakenN :: WeakenedN n a -> Either StrengthenFailure' a

instance {-# OVERLAPPING #-} StrengthenWeakenN 0 a where
    strengthenWeakenN = Right

instance (Strengthen a, StrengthenWeakenN (n-1) (Weakened a))
  => StrengthenWeakenN n a where
    strengthenWeakenN a =
        case strengthenWeakenN @(n-1) @(Weakened a) (weakenedNLR1 @n @a a) of
          Left  e  -> Left e
          Right sa -> strengthen sa

-- | Inductive 'WeakenedN'case.
--
-- @n@ must not be 0.
weakenedNLR1 :: forall n a. WeakenedN n a -> WeakenedN (n-1) (Weakened a)
weakenedNLR1 = unsafeCoerce
