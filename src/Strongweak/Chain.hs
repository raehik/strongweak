{-# LANGUAGE UndecidableInstances #-} -- type family 'Weakened' in constraints
{-# LANGUAGE AllowAmbiguousTypes #-} -- ambiguous intermediate type classes

module Strongweak.Chain where

import Strongweak.Weaken ( Weaken(weaken), type WeakenedN )
import Strongweak.Strengthen
import GHC.TypeNats
import Unsafe.Coerce ( unsafeCoerce )

{- | When weakening (or strengthening), chain the operation @n@ times.

You may achieve this without extra newtypes by nesting uses of
'Strongweak.Weaken.SW'. However, strongweak generics can't handle this, forcing
you to write manual instances.

'SWChain' provides this nesting behaviour in a type. In return for adding a
boring newtype layer to the strong representation, you can chain weakening and
strengthenings without having to write them manually.

The type works as follows:

@
Weakened (SWChain 0 a) = a
Weakened (SWChain 1 a) = Weakened a
Weakened (SWChain 2 a) = Weakened (Weakened a)
Weakened (SWChain n a) = WeakenedN n a
@

And so on. (This type is only much use from @n = 2@ onwards.)
-}
newtype SWChain (n :: Natural) a = SWChain { unSWChain :: a }
    deriving stock Show

instance WeakenN n a => Weaken (SWChain n a) where
    type Weakened (SWChain n a) = WeakenedN n a
    weaken = weakenN @n @a . unSWChain

class WeakenN (n :: Natural) a where
    weakenN :: a -> WeakenedN n a

-- | Zero case: return the value as-is.
--
-- TODO The overlapping rules always confuse me. @OVERLAPPING@ is right, right?
instance {-# OVERLAPPING #-} WeakenN 0 a where
    weakenN = id

-- | Inductive case. @n /= 0@, else this explodes.
instance (Weaken a, WeakenN (n-1) (Weakened a)) => WeakenN n a where
    weakenN a =
        case weakenN @(n-1) @(Weakened a) (weaken a) of
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

instance StrengthenN n a => Strengthen (SWChain n a) where
    strengthen = fmap SWChain . strengthenN @n @a

class WeakenN n a => StrengthenN (n :: Natural) a where
    strengthenN :: WeakenedN n a -> Either StrengthenFailure' a

instance {-# OVERLAPPING #-} StrengthenN 0 a where
    strengthenN = Right

instance (Strengthen a, StrengthenN (n-1) (Weakened a))
  => StrengthenN n a where
    strengthenN a =
        case strengthenN @(n-1) @(Weakened a) (weakenedNLR1 @n @a a) of
          Left  e  -> Left e
          Right sa -> strengthen sa

{-

instance Weaken (Chain 0 a) where
    type Weakened (Chain 0 a) = a
    weaken = unChain
instance Strengthen (Chain 0 a) where
    strengthen = Right . Chain

instance Weaken a => Weaken (Chain 1 a) where
    type Weakened (Chain 1 a) = Weakened a
    weaken = weaken . unChain
instance Strengthen a => Strengthen (Chain 1 a) where
    strengthen = fmap Chain . strengthen

instance (Weaken a, Weaken (Weakened a)) => Weaken (Chain 2 a) where
    type Weakened (Chain 2 a) = Weakened (Weakened a)
    weaken = weaken . weaken . unChain
instance (Strengthen a, Strengthen (Weakened a)) => Strengthen (Chain 2 a) where
    strengthen a = -- TODO how to pointfree this lol? pointfree fmap confuses me
        case strengthen a of
          Left  e  -> Left e
          Right sa -> fmap Chain (strengthen sa)
-}

{-
newtype Twice a = Twice { unTwice :: a }
    deriving stock Show
instance (Weaken a, Weaken (Weakened a)) => Weaken (Twice a) where
    type Weakened (Twice a) = Weakened (Weakened a)
    weaken = weaken . weaken . unTwice
-}
