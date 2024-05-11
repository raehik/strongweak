module Strongweak.Strengthen.Unsafe where

import Strongweak.Weaken
import Data.Word
import Data.Int
import Rerefined.Refine
import Data.Vector.Generic.Sized qualified as VGS -- Shazbot!
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Sized.Internal qualified
import Data.Functor.Identity
import Data.Functor.Const
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty ( NonEmpty )

{- | Unsafely transform a @'Weak' a@ to an @a@, without asserting invariants.

Naturally, you must only even /consider/ using this if you have a guarantee that
your value is safe to treat as strong.

For example, you may unsafely strengthen some @'Numeric.Natural.Natural' n@ into
a 'Word8' by unsafely coercing the value, ignoring the possibility that @n >=
255@.

What happens if it turns out you're lying to the computer and your weak value
doesn't fit in its strong counterpart? That depends on the strengthen.

  * Numeric coercions should safely overflow.
  * Some will raise an error (e.g. 'NonEmpty').
  * Others will appear to work, but later explode your computer.

See "Strongweak" for class design notes and laws.
-}
class Weaken a => UnsafeStrengthen a where
    -- | Unsafely transform a @'Weak' a@ to its associated strong type @a@.
    unsafeStrengthen :: Weak a -> a

-- | Add a refinement to a type without checking the associated predicate.
instance UnsafeStrengthen (Refined p a) where
    unsafeStrengthen = unsafeRefine

-- | Assume a plain list is non-empty.
instance UnsafeStrengthen (NonEmpty a) where
    unsafeStrengthen = NonEmpty.fromList

-- | Assume the size of a plain list.
instance VG.Vector v a => UnsafeStrengthen (VGS.Vector v n a) where
    unsafeStrengthen =
        Data.Vector.Generic.Sized.Internal.Vector . VG.fromList

-- | Add wrapper.
instance UnsafeStrengthen (Identity a) where
    unsafeStrengthen = Identity

-- | Add wrapper.
instance UnsafeStrengthen (Const a b) where
    unsafeStrengthen = Const

{- TODO controversial. seems logical, but also kinda annoying.
-- | Unsafely grab either 0 or 1 elements from a list.
instance UnsafeStrengthen (Maybe a) where
    unsafeStrengthen = \case [a] -> Just a
                             []  -> Nothing
                             _   -> error "your list wasn't [] or [a]"
-}

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

-- | Decomposer. Unsafely strengthen both elements of a tuple.
instance (UnsafeStrengthen a, UnsafeStrengthen b)
  => UnsafeStrengthen (a, b) where
    unsafeStrengthen (a, b) = (unsafeStrengthen a, unsafeStrengthen b)

-- | Decomposer. Unsafely strengthen either side of an 'Either'.
instance (UnsafeStrengthen a, UnsafeStrengthen b)
  => UnsafeStrengthen (Either a b) where
    unsafeStrengthen = \case Left  a -> Left  $ unsafeStrengthen a
                             Right b -> Right $ unsafeStrengthen b
