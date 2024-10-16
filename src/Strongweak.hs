{- | Main import module for basic use.

For defining 'Strengthen' instances, import "Strongweak.Strengthen".
-}

module Strongweak
  (
  -- * Instance design
  -- $strongweak-instance-design

  -- * Classes
    Weaken(..)
  , Strengthen(..)

  -- * Other definitions
  , liftWeakF

  -- * Strength switch wrapper
  , Strength(..)
  , type SW

  ) where

import Strongweak.Weaken
import Strongweak.Strengthen

{- $strongweak-instance-design

A given strong type @a@ has exactly one associated weak type @'Weakened' a@.
Multiple strong types may weaken to the same weak type.

The following laws must hold:

  * @'weaken' a == 'weaken' b |= a == b@
  * @'strengthen' ('weaken' a) == 'pure' a@

strongweak is largely a programmer convenience library. There is a lot of room
to write instances which may seem useful on first glance, but are inconsistent
with the overall design. Here is some relevant guidance.

  * Weak types should have _simpler invariants to manage_ than strong ones.
  * In general, weak types should be easier to use than strong ones.
  * Most (all?) instances should handle (relax or assert) a single invariant.
  * Most instances should not have a recursive context.

If you want to handle multiple invariants, chain the weakens/strengthens.
You may do this by nesting 'SW' uses, but you will then have to write your own
instances, as the generics cannot handle such chaining. Alternatively, you may
use 'SWChain'. This will add another newtype layer to the strong representation,
but the generics are happy with it.

Some types may not have any invariants which may be usefully relaxed e.g.
@'Either' a b@. For these, you may write a recursive instance that
weakens/strengthens "through" the type e.g. @('Weak' a, 'Weak' b) => Weak
('Either' a b)@). Don't combine the two instance types.

An example is @'Data.List.NonEmpty.NonEmpty' a@. We could weaken this to @[a]@,
but also to @['Weak' a]@. However, the latter would mean decomposing and
removing an invariant simultaneously. It would be two separate strengthens in
one instance. And now, your 'a' must be in the strongweak ecosystem, which isn't
necessarily what you want - indeed, it appears this sort of design would require
a @'Weak' a = a, weaken = id@ overlapping instance, which I do not want. On the
other hand, @[a]@ /does/ weaken to @['Weak' a]@, because there are no invariants
present to remove, so decomposing is all the user could hope to do.

Another problem is coercible newtypes such as @Tagged@ from the @tagged@
package. These have no invariants for us to weaken, so one's knee-jerk reaction
might be to weaken the inner type. But what if the user doesn't want to weaken
that inner type? They might be happy with using 'Data.Word.Word' instead of
'Numeric.Natural.Natural', for example.

strongweak provides a special type 'Coercibly' which permits precisely defining
how to weaken a newtype/pair of coercible types. For this reason, zero-invariant
coercible newtypes are excluded from the strongweak ecosystem (e.g.
'Data.Functor.Identity.Identity', 'Data.Functor.Const.Const', @Tagged@).

If your newtype puts the wrapped type as the final type variable, use
'Coercibly1' for a considerably simpler invocation.
-}
