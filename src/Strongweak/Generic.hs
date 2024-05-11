{-# LANGUAGE UndecidableInstances #-} -- required due to nested constraints

-- | Generic 'strengthen' and 'weaken'.

module Strongweak.Generic
  (
  -- * Generic derivation compatibility
  -- $generic-derivation-compatibility

  -- * Generic derivers
    weakenGeneric
  , strengthenGeneric

  -- * Generic wrapper
  , GenericallySW(..)
  ) where

import Strongweak.Weaken.Generic
import Strongweak.Strengthen.Generic

import Strongweak.Weaken ( Weaken(Weak, weaken) )
import Strongweak.Strengthen ( Strengthen(strengthen) )
import GHC.Generics
import Data.Kind ( Type )

{- $generic-derivation-compatibility

The 'Strengthen' and 'Weaken' generic derivers allow you to derive instances
between any /compatible/ pair of types. Compatibility is defined as follows:

  * Both types' generic representation (the SOP tree structure) match exactly.
  * For each leaf pair of types, either the types are identical, or the
  appropriate instance exists to transform from source to target.

If they aren't compatible, the derivation will fail with a type error. I'm
fairly certain that if it succeeds, your instance is guaranteed correct
(assuming the instances it uses internally are all OK!).

I don't think GHC strongly guarantees the SOP property, so if you receive
surprising derivation errors, the types might have differing generic
representation structure, even if their flattened representations are identical.
If you experience this let me know, since in my experience GHC's stock @Generic@
derivation is highly deterministic.

Also, generic strengthening requires that all metadata is present for both
types: for the datatype, constructors and selectors. GHC will always add this
metadata for you, but manually-derived Generic instances (which are usually a
bad idea) do not require it.
-}

{- | @DerivingVia@ wrapper for strongweak instances.

We can't use 'Generically' conveniently because we need to talk about two data
types, not one -- we would have to do something like @'Generically' ('Tagged' w
s)@, which is ugly. So we instead define our own adorable little "via type"
here!

Use like so:

@
data XYZ (s :: Strength) = XYZ
  { xyz1 :: SW s Word8
  , xyz2 :: Word8
  , xyz3 :: ()
  } deriving stock Generic
deriving via (GenericallySW (XYZ 'Strong) (XYZ 'Weak)) instance Weaken (XYZ 'Strong)
deriving via (GenericallySW (XYZ 'Strong) (XYZ 'Weak)) instance Strengthen (XYZ 'Strong)
@

TODO can't figure out a way around multiple standalone deriving declarations :(

TODO maybe GenericallySW1? but even so instances differ between weaken and
strengthen (weaken needs nothing) so it's kinda better this way. :)
-}

newtype GenericallySW s (w :: Type) = GenericallySW { unGenericallySW :: s }

instance
  ( Generic s, Generic w
  , GWeaken (Rep s) (Rep w)
  ) => Weaken (GenericallySW s w) where
    type Weak (GenericallySW s w) = w
    weaken = weakenGeneric . unGenericallySW

instance
  ( Generic s, Generic w
  , GStrengthenD (Rep w) (Rep s)
  , Weaken (GenericallySW s w)
  ) => Strengthen (GenericallySW s w) where
    strengthen = fmap GenericallySW . strengthenGeneric
