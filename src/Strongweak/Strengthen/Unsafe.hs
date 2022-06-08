module Strongweak.Strengthen.Unsafe where

import Strongweak.Weaken
import Data.Word
import Data.Int
import Refined ( Refined )
import Refined.Unsafe ( reallyUnsafeRefine )
import Data.Vector.Sized ( Vector )
import Data.Vector.Generic.Sized.Internal qualified
import Data.Vector qualified
import Data.Functor.Identity
import Data.Functor.Const
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty ( NonEmpty )

{- | Unsafely transform a @'Weak' a@ to an @a@, without asserting invariants.

For example, you may unsafely strengthen some @'Numeric.Natural.Natural' n@ into
a 'Word8' by unsafely coercing the value, ignoring the possibility that @n >=
255@.

What happens if it turns out you're lying to the computer and your weak value
doesn't fit in its strong counterpart? That depends on the strengthen.

  * Numeric coercions should safely overflow.
  * Some will raise an error (e.g. 'NonEmpty').
  * Others will appear to work, but later explode your computer (sized vectors
    will probably do this).

Only consider using this if you have a guarantee that your value is safe to
treat as strong.

Instances should /either/ handle an invariant, or decompose. See "Strongweak"
for a discussion on this design.
-}
class Weaken a => UnsafeStrengthen a where
    -- | Unsafely transform a weak value to its associated strong one.
    unsafeStrengthen :: Weak a -> a

-- | Unsafely assume a list is non-empty.
instance UnsafeStrengthen (NonEmpty a) where
    unsafeStrengthen = NonEmpty.fromList

-- | Unsafely assume the size of a plain list.
instance UnsafeStrengthen (Vector n a) where
    unsafeStrengthen = Data.Vector.Generic.Sized.Internal.Vector . Data.Vector.fromList

-- | Wrap a value to a refined one without checking the predicate.
instance UnsafeStrengthen (Refined p a) where
    unsafeStrengthen = reallyUnsafeRefine

-- Coerce 'Natural's into Haskell's bounded unsigned numeric types. Poorly-sized
-- values will safely overflow according to the type's behaviour.
instance UnsafeStrengthen Word8  where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Word16 where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Word32 where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Word64 where unsafeStrengthen = fromIntegral

-- Coerce 'Integer's into Haskell's bounded signed numeric types. Poorly-sized
-- values will safely overflow according to the type's behaviour.
instance UnsafeStrengthen Int8   where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Int16  where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Int32  where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Int64  where unsafeStrengthen = fromIntegral

--------------------------------------------------------------------------------

-- | Decomposer. Unsafely strengthen every element in a list.
instance UnsafeStrengthen a => UnsafeStrengthen [a] where
    unsafeStrengthen = map unsafeStrengthen

-- | Decomposer.
instance (UnsafeStrengthen a, UnsafeStrengthen b) => UnsafeStrengthen (a, b) where
    unsafeStrengthen (a, b) = (unsafeStrengthen a, unsafeStrengthen b)

-- | Decomposer.
instance UnsafeStrengthen a => UnsafeStrengthen (Maybe a) where
    unsafeStrengthen = \case Just a  -> Just $ unsafeStrengthen a
                             Nothing -> Nothing

-- | Decomposer.
instance (UnsafeStrengthen a, UnsafeStrengthen b) => UnsafeStrengthen (Either a b) where
    unsafeStrengthen = \case Left  a -> Left  $ unsafeStrengthen a
                             Right b -> Right $ unsafeStrengthen b

-- | Decomposer.
instance UnsafeStrengthen a => UnsafeStrengthen (Identity a) where
    unsafeStrengthen = Identity . unsafeStrengthen . runIdentity

-- | Decomposer.
instance UnsafeStrengthen a => UnsafeStrengthen (Const a b) where
    unsafeStrengthen = Const . unsafeStrengthen . getConst
