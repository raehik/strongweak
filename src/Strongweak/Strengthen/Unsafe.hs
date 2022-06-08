module Strongweak.Strengthen.Unsafe where

import Strongweak.Weaken
import Data.Word
import Data.Int
import Refined ( Refined )
import Refined.Unsafe ( reallyUnsafeRefine )
import Data.Vector.Sized ( Vector )
import Data.Vector.Generic.Sized.Internal qualified
import Data.Vector qualified

{- | Unsafely transform a @'Weak' a@ to an @a@, without asserting invariants.

For example, you may unsafely strengthen some @'Numeric.Natural.Natural' n@ into
a 'Word8' by unsafely coercing the value, ignoring the possibility that @n >=
255@.

Some unsafe strengthens are more dangerous than others. The above one would
safely overflow on invalid inputs, but others will explode your computer if it
turns out you were lying and an invariant wasn't upheld. Only consider using
this if you have a guarantee that your value is safe to treat as strong.
-}
class Weaken a => UnsafeStrengthen a where
    -- | Unsafely transform a weak value to its associated strong one.
    unsafeStrengthen :: Weak a -> a

-- | Unsafely strengthen each element of a list.
instance UnsafeStrengthen a => UnsafeStrengthen [a] where
    unsafeStrengthen = map unsafeStrengthen

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
