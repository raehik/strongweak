{-# LANGUAGE FunctionalDependencies #-}

module Strongweak.Strengthen.Unsafe where

import Numeric.Natural
import Data.Word
import Data.Int
import Refined ( Refined )
import Refined.Unsafe ( reallyUnsafeRefine )
import Data.Vector.Sized ( Vector )
import Data.Vector.Generic.Sized.Internal qualified
import Data.Vector qualified

{- | Any 'w' can be unsafely "strengthened" into an 's' by pretending that we've
     asserted some properties.

For example, you may unsafely strengthen some 'Natural' @n@ into a 'Word8' by
unsafely coercing the value, ignoring the possibility that @n >= 255@.

Currently, this class is more of a thought experiment than something to use.
That is to say, do not use this.

This typeclass should probably follow its big sis 'Strengthen'. Only provide
'UnsafeStrengthen' instances for types that have similar 'Strengthen' instances.
-}
class UnsafeStrengthen w s | s -> w where unsafeStrengthen :: w -> s

-- | 'unsafeStrengthen' with reordered type variables for more convenient
--   visible type application.
unsafeStrengthen' :: forall s w. UnsafeStrengthen w s => w -> s
unsafeStrengthen' = unsafeStrengthen

-- | Unsafely strengthen each element of a list.
instance UnsafeStrengthen w s => UnsafeStrengthen [w] [s] where
    unsafeStrengthen = map unsafeStrengthen

-- | Obtain a sized vector by unsafely assuming the size of a plain list.
instance UnsafeStrengthen [a] (Vector n a) where
    unsafeStrengthen = Data.Vector.Generic.Sized.Internal.Vector . Data.Vector.fromList

-- | Obtain a refined type by ignoring the predicate.
instance UnsafeStrengthen a (Refined p a) where
    unsafeStrengthen = reallyUnsafeRefine

-- Coerce 'Natural's into Haskell's bounded unsigned numeric types.
instance UnsafeStrengthen Natural Word8  where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Natural Word16 where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Natural Word32 where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Natural Word64 where unsafeStrengthen = fromIntegral

-- Coerce 'Integer's into Haskell's bounded signed numeric types.
instance UnsafeStrengthen Integer Int8   where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Integer Int16  where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Integer Int32  where unsafeStrengthen = fromIntegral
instance UnsafeStrengthen Integer Int64  where unsafeStrengthen = fromIntegral
