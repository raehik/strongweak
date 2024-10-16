{-# LANGUAGE UndecidableInstances #-} -- for WeakenedN

module Strongweak.Weaken
  (
  -- * 'Weaken' class
    Weaken(..)
  , type WeakenedN
  , liftWeakF

  -- * Strength switch helper
  , Strength(..)
  , type SW

  , ErrZeroInvariantNewtype'
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

import GHC.TypeError
import Strongweak.Util.TypeErrors
import GHC.TypeLits ( type Symbol )

{- | Weaken some @a@, relaxing certain invariants.

See "Strongweak" for class design notes and laws.
-}
class Weaken a where
    -- | The weakened type for some type.
    type Weakened a :: Type

    -- | Weaken some @a@ to its associated weak type @'Weakened' a@.
    weaken :: a -> Weakened a

-- | Strength enumeration: is it strong, or weak?
--
-- Primarily interesting at the type level (using DataKinds).
data Strength = Strong | Weak

-- | Lift a function on a weak type to the associated strong type by weakening
--   first.
liftWeakF :: Weaken a => (Weakened a -> b) -> a -> b
liftWeakF f = f . weaken

{- | Get either the strong or weak representation of a type, depending on the
     type-level "switch" provided.

This is intended to be used in data types that take a 'Strength' type. Define
your type using strong fields wrapped in @SW s@. You then get the weak
representation for free, using the same definition.

@
data A (s :: Strength) = A
  { a1 :: SW s Word8
  , a2 :: String }
@
-}
type family SW (s :: Strength) a :: Type where
    SW Strong a =          a
    SW Weak   a = Weakened a

-- | The type of @a@ after weakening @n@ times.
type family WeakenedN (n :: Natural) a :: Type where
    WeakenedN 0 a = a
    WeakenedN n a = Weakened (WeakenedN (n-1) a)

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

---

newtype ErrZeroInvariantNewtype' (typeName :: Symbol) a
  = ErrZeroInvariantNewtype' a
instance Unsatisfiable (ErrZeroInvariantNewtype typeName)
  => Weaken (ErrZeroInvariantNewtype' typeName a) where
    type Weakened (ErrZeroInvariantNewtype' typeName a) =
        TypeError (ErrZeroInvariantNewtype typeName)
    weaken = unsatisfiable

--deriving via ErrZeroInvariantNewtype' "Identity" a
--  instance Weaken (Identity a)

{- TODO 2024-10-16T04:21:22+0100
aww this doesn't work haha. ok fine just gotta make some utils for filling out
the context and Weakened associated type family for custom erroring instances
-}

{-
instance Unsatisfiable (ErrZeroInvariantNewtype "Identity")
  => Weaken (Identity a) where
    type Weakened (Identity a) = a
    weaken = unsatisfiable
-}

-- TODO define custom errors using Unsatisfiable to point users to Coercibly
-- Unsatisfiable is base-4.19 -> GHC 9.8
--deriving via Coercibly1 Shallow Identity    a instance Weaken (Identity a)
--deriving via Coercibly  Shallow (Const a b) a instance Weaken (Const a b)

instance Weaken (Identity a) where
    type Weakened (Identity a) = a
    weaken (Identity a) = a
