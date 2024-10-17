{-# LANGUAGE UndecidableInstances #-} -- for WeakenedN
{-# LANGUAGE AllowAmbiguousTypes #-} -- for weakenN

module Strongweak.Weaken
  (
    Weaken(Weakened, weaken)
  , type WeakenedN
  , WeakenN(weakenN)
  , liftWeakF
  , SWCoercibly(..)
  ) where

import Rerefined
import Data.Word
import Data.Int
import Data.Vector.Generic.Sized qualified as VGS -- Shazbot!
import Data.Vector.Generic qualified as VG
import Data.Kind ( Type )
import Data.Functor.Identity
import Data.Functor.Const
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty ( NonEmpty )
import GHC.TypeNats

import Unsafe.Coerce ( unsafeCoerce )

{- | Weaken some @a@, relaxing certain invariants.

See "Strongweak" for class design notes and laws.
-}
class Weaken a where
    -- | The weakened type for some type.
    type Weakened a :: Type

    -- | Weaken some @a@ to its associated weak type @'Weakened' a@.
    weaken :: a -> Weakened a

-- | Lift a function on a weak type to the associated strong type by weakening
--   first.
liftWeakF :: Weaken a => (Weakened a -> b) -> a -> b
liftWeakF f = f . weaken

-- | The type of @a@ after weakening @n@ times.
type family WeakenedN (n :: Natural) a :: Type where
    WeakenedN 0 a = a
    WeakenedN n a = Weakened (WeakenedN (n-1) a)

-- | A "via type" newtype for defining strongweak instances for zero-invariant,
--   coercible newtypes.
--
-- Use like so:
--
-- @
-- deriving via 'SWCoercibly' a instance 'Weaken' ('Identity' a)
-- @
newtype SWCoercibly a = SWCoercibly { unSWCoercibly :: a }
instance Weaken (SWCoercibly a) where
    type Weakened (SWCoercibly a) = a
    weaken = unSWCoercibly

deriving via SWCoercibly a instance Weaken (Identity a)
deriving via SWCoercibly a instance Weaken (Const a b)

-- | Strip refined type refinement.
instance Weaken   (Refined p a) where
    type Weakened (Refined p a) = a
    weaken = unrefine

-- | Strip refined functor type refinement.
instance Weaken   (Refined1 p f a) where
    type Weakened (Refined1 p f a) = f a
    weaken = unrefine1

-- | Weaken non-empty lists into plain lists.
instance Weaken   (NonEmpty a) where
    type Weakened (NonEmpty a) = [a]
    weaken = NonEmpty.toList

-- | Weaken sized vectors into plain lists.
instance VG.Vector v a => Weaken (VGS.Vector v n a) where
    type Weakened (VGS.Vector v n a) = [a]
    weaken = VGS.toList

{- TODO controversial. seems logical, but also kinda annoying.
-- | Weaken 'Maybe' (0 or 1) into '[]' (0 to n).
instance Weaken (Maybe a) where
    type Weakened (Maybe a) = [a]
    weaken = \case Just a  -> [a]
                   Nothing -> []
-}

-- Weaken the bounded Haskell numeric types using 'fromIntegral'.
instance Weaken   Word8  where
    type Weakened Word8  = Natural
    weaken = fromIntegral
instance Weaken   Word16 where
    type Weakened Word16 = Natural
    weaken = fromIntegral
instance Weaken   Word32 where
    type Weakened Word32 = Natural
    weaken = fromIntegral
instance Weaken   Word64 where
    type Weakened Word64 = Natural
    weaken = fromIntegral
instance Weaken   Int8   where
    type Weakened Int8   = Integer
    weaken = fromIntegral
instance Weaken   Int16  where
    type Weakened Int16  = Integer
    weaken = fromIntegral
instance Weaken   Int32  where
    type Weakened Int32  = Integer
    weaken = fromIntegral
instance Weaken   Int64  where
    type Weakened Int64  = Integer
    weaken = fromIntegral

--------------------------------------------------------------------------------

-- | Decomposer. Weaken every element in a list.
instance Weaken a => Weaken [a] where
    type Weakened [a] = [Weakened a]
    weaken = map weaken

-- | Decomposer. Weaken both elements of a tuple.
instance (Weaken a, Weaken b) => Weaken (a, b) where
    type Weakened (a, b) = (Weakened a, Weakened b)
    weaken (a, b) = (weaken a, weaken b)

-- | Decomposer. Weaken either side of an 'Either'.
instance (Weaken a, Weaken b) => Weaken (Either a b) where
    type Weakened (Either a b) = Either (Weakened a) (Weakened b)
    weaken = \case Left  a -> Left  $ weaken a
                   Right b -> Right $ weaken b

class WeakenN (n :: Natural) a where
    weakenN :: a -> WeakenedN n a

-- | Zero case: return the value as-is.
instance {-# OVERLAPPING #-} WeakenN 0 a where
    weakenN = id

-- | Inductive case. @n /= 0@, else this explodes.
instance (Weaken a, WeakenN (n-1) (Weakened a))
  => WeakenN n a where
    weakenN a =
        case weakenN @(n-1) @(Weakened a) (weaken a) of
          x -> weakenedNRL1 @n @a x

-- | Inverted inductive 'WeakenedN'case.
--
-- @n@ must not be 0.
weakenedNRL1 :: forall n a. WeakenedN (n-1) (Weakened a) -> WeakenedN n a
weakenedNRL1 = unsafeCoerce
