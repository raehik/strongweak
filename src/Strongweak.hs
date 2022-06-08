module Strongweak
  (
  -- * Instance design
  -- $strongweak-instance-design

  -- * Re-exports
    module Strongweak.Weaken
  , module Strongweak.Strengthen
  ) where

import Strongweak.Weaken
import Strongweak.Strengthen

{- $strongweak-instance-design

We identify two distinct types of instances for strongweak classes:

  * /invariant handler:/ removes or adds an invariant
  * /decomposer:/ transforms through some structural type

In order to provide good behaviour and composability, we don't mix both in a
single instance. The decomposers are really just convenience to ease instance
derivation. In general, decomposers will have a recursive context, and invariant
handlers won't.

An example is @'Data.List.NonEmpty.NonEmpty' a@. We could weaken this to @[a]@,
but also to @['Weak' a]@. However, the latter would mean decomposing and
removing an invariant simultaneously. It would be two separate strengthens in
one instance. And now, your 'a' must be in the strongweak ecosystem, which isn't
necessarily what you want - indeed, it appears this sort of design would require
a @'Weak' a = a, weaken = id@ overlapping instance, which I do not want. On the
other hand, @[a]@ /does/ weaken to @['Weak' a]@, because there are no invariants
present to remove, so decomposing is all the user could hope to do.

-}
