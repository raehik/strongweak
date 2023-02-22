module Strongweak.Weaken
  (
  -- * 'Weaken' class
    Weaken(..)
  , liftWeakF

  -- * 'SW' helper
  , Strength(..)
  , SW
  ) where

import Refined ( Refined, unrefine )
import Numeric.Natural ( Natural )
import Data.Word
import Data.Int
import Data.Vector.Generic.Sized qualified as VGS -- Shazbot!
import Data.Vector.Generic qualified as VG
import Data.Kind ( Type )
import Data.Functor.Identity
import Data.Functor.Const
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty ( NonEmpty )

{- | Weaken some @a@, relaxing certain invariants.

See "Strongweak" for class design notes and laws.
-}
class Weaken a where
    -- | The weakened type for some type.
    type Weak a :: Type

    -- | Weaken some @a@ to its associated weak type @'Weak' a@.
    weaken :: a -> Weak a

-- | Lift a function on a weak type to the associated strong type.
liftWeakF :: Weaken a => (Weak a -> b) -> (a -> b)
liftWeakF f = f . weaken

-- | Strength enumeration: is it strong, or weak?
--
-- Primarily interesting at the type level (using DataKinds).
data Strength = Strong | Weak

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
    SW 'Strong a = a
    SW 'Weak   a = Weak a

-- | Strip refined type refinement.
instance Weaken (Refined p a) where
    type Weak (Refined p a) = a
    weaken = unrefine

-- | Weaken non-empty lists into plain lists.
instance Weaken (NonEmpty a) where
    type Weak (NonEmpty a) = [a]
    weaken = NonEmpty.toList

-- | Weaken sized vectors into plain lists.
instance VG.Vector v a => Weaken (VGS.Vector v n a) where
    type Weak (VGS.Vector v n a) = [a]
    weaken = VGS.toList

-- | Strip wrapper.
instance Weaken (Identity a) where
    type Weak (Identity a) = a
    weaken = runIdentity

-- | Strip wrapper.
instance Weaken (Const a b) where
    type Weak (Const a b) = a
    weaken = getConst

{- TODO controversial. seems logical, but also kinda annoying.
-- | Weaken 'Maybe' (0 or 1) into '[]' (0 to n).
instance Weaken (Maybe a) where
    type Weak (Maybe a) = [a]
    weaken = \case Just a  -> [a]
                   Nothing -> []
-}

-- Weaken the bounded Haskell numeric types using 'fromIntegral'.
instance Weaken Word8  where
    type Weak Word8  = Natural
    weaken = fromIntegral
instance Weaken Word16 where
    type Weak Word16 = Natural
    weaken = fromIntegral
instance Weaken Word32 where
    type Weak Word32 = Natural
    weaken = fromIntegral
instance Weaken Word64 where
    type Weak Word64 = Natural
    weaken = fromIntegral
instance Weaken Int8   where
    type Weak Int8   = Integer
    weaken = fromIntegral
instance Weaken Int16  where
    type Weak Int16  = Integer
    weaken = fromIntegral
instance Weaken Int32  where
    type Weak Int32  = Integer
    weaken = fromIntegral
instance Weaken Int64  where
    type Weak Int64  = Integer
    weaken = fromIntegral

--------------------------------------------------------------------------------

-- | Decomposer. Weaken every element in a list.
instance Weaken a => Weaken [a] where
    type Weak [a] = [Weak a]
    weaken = map weaken

-- | Decomposer. Weaken both elements of a tuple.
instance (Weaken a, Weaken b) => Weaken (a, b) where
    type Weak (a, b) = (Weak a, Weak b)
    weaken (a, b) = (weaken a, weaken b)

-- | Decomposer. Weaken either side of an 'Either'.
instance (Weaken a, Weaken b) => Weaken (Either a b) where
    type Weak (Either a b) = Either (Weak a) (Weak b)
    weaken = \case Left  a -> Left  $ weaken a
                   Right b -> Right $ weaken b
