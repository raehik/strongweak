{-# LANGUAGE UndecidableInstances #-} -- type family 'Weakened' in constraints
{-# LANGUAGE AllowAmbiguousTypes #-} -- ambiguous intermediate type classes

module Strongweak.WeakenN
  ( WeakenN(..)
  , type SWN
  ) where

import Strongweak.Weaken ( Weaken(weaken), type WeakenedN )
import Strongweak.Strengthen
import GHC.TypeNats ( type Natural, type (-) )
import Unsafe.Coerce ( unsafeCoerce )

import Strongweak.Strength ( type Strength, type SW )

{- | When weakening (or strengthening), chain the operation @n@ times.

You may achieve this without extra newtypes by nesting uses of
'Strongweak.Weaken.SW'. However, strongweak generics can't handle this, forcing
you to write manual instances.

'WeakenN' provides this nesting behaviour in a type. In return for adding a
boring newtype layer to the strong representation, you can chain weakening and
strengthenings without having to write them manually.

The type works as follows:

@
Weakened (WeakenN 0 a) = a
Weakened (WeakenN 1 a) = Weakened a
Weakened (WeakenN 2 a) = Weakened (Weakened a)
Weakened (WeakenN n a) = WeakenedN n a
@

And so on. (This type is only much use from @n = 2@ onwards.)
-}
newtype WeakenN (n :: Natural) a = WeakenN { unWeakenN :: a }
    deriving stock Show

-- | Shortcut for a 'SW'-wrapped 'WeakenN'.
type SWN (s :: Strength) (n :: Natural) a = SW s (WeakenN n a)

instance WeakenWeakenN n a => Weaken (WeakenN n a) where
    type Weakened (WeakenN n a) = WeakenedN n a
    weaken = weakenWeakenN @n @a . unWeakenN

class WeakenWeakenN (n :: Natural) a where
    weakenWeakenN :: a -> WeakenedN n a

-- | Zero case: return the value as-is.
--
-- TODO The overlapping rules always confuse me. @OVERLAPPING@ is right, right?
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

-- | Inductive 'WeakenedN'case.
--
-- @n@ must not be 0.
weakenedNLR1 :: forall n a. WeakenedN n a -> WeakenedN (n-1) (Weakened a)
weakenedNLR1 = unsafeCoerce

instance StrengthenWeakenN n a => Strengthen (WeakenN n a) where
    strengthen = fmap WeakenN . strengthenWeakenN @n @a

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
