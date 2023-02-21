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
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized ( Vector )
import Data.Kind ( Type )
import Data.Functor.Identity
import Data.Functor.Const
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty ( NonEmpty )

{- | Transform an @a@ to a @'Weak' a@.

A given strong type @a@ has exactly one associated weak type @'Weak' a@.
Multiple strong types may weaken to the same weak type.

Law: @a === b -> 'weaken' a === 'weaken' b@

^ TODO uhhhhhh is that correct??? shouldn't it be the OTHER WAY AROUND???????

Instances should /either/ handle an invariant, or decompose. See "Strongweak"
for a discussion on this design.
-}
class Weaken a where
    -- | The type to weaken to.
    type Weak a :: Type

    -- | Transform a strong value to its associated weak one.
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

-- | Weaken non-empty lists into plain lists.
instance Weaken (NonEmpty a) where
    type Weak (NonEmpty a) = [a]
    weaken = NonEmpty.toList

-- | Weaken sized vectors into plain lists.
instance Weaken (Vector n a) where
    type Weak (Vector n a) = [a]
    weaken = Vector.toList

-- | Strip the refinement from refined types.
instance Weaken (Refined p a) where
    type Weak (Refined p a) = a
    weaken = unrefine

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

-- | Decomposer.
instance (Weaken a, Weaken b) => Weaken (a, b) where
    type Weak (a, b) = (Weak a, Weak b)
    weaken (a, b) = (weaken a, weaken b)

instance Weaken (Maybe a) where
    type Weak (Maybe a) = [a]
    weaken = \case Just a  -> [a]
                   Nothing -> []

-- | Decomposer.
instance (Weaken a, Weaken b) => Weaken (Either a b) where
    type Weak (Either a b) = Either (Weak a) (Weak b)
    weaken = \case Left  a -> Left  $ weaken a
                   Right b -> Right $ weaken b

instance Weaken (Identity a) where
    type Weak (Identity a) = a
    weaken = runIdentity

instance Weaken (Const a b) where
    type Weak (Const a b) = a
    weaken = getConst
