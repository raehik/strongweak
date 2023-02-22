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

import Strongweak.Generic.Weaken
import Strongweak.Generic.Strengthen
import Strongweak.Generic.Via

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
