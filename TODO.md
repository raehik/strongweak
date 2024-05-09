# strongweak to-dos
* split into base definitions and orphan instances?
  * base needs either, acc, text and prettyprinter

## Failures: is Typeable really the right choice?
I probably went with `TypeRep`s for filling out failure detail due to copying
refined's design. With rerefined, I've gone back on that, and I now
don't use `Typeable` at all. That said, is it the right pick here?

* The generics don't use it at all.
* Concrete instances could use an associated type family like `type Name =
  "Word32"`.
  * And strongweak largely deals in concrete instances.
* We can do the same type-level `Show` stuff that rerefined does.
  * And we can embed `PredicateName` for `Refined[1]` instances.

The user will have to fill out a bunch of associated type families when using
strongweak, which sucks. Or maybe we can do a shitty job automatically for
generics? Like we can get the datatype name-- no other info, but that's a good
start lol.

The `Show`ing feels a little weird too, but I think it's just that we get away
with it thanks to having lots of concrete instances. Non-concrete instances
would drop it.

Hmm. Not too sure how `Refined` instances will look. How do we get a `Symbol`
from the `a` in `Refined p a`? We can say "it's some type refined with this
predicate", but we apparently can't tell you the type.

Hold on, we went with type-level show for predicates because we were doing lots
of combination. We do no such thing here. Why don't we just write in the
`TypeRep`s ourselves? Easy for concrete, fine for `Refined`. Apparently not a
problem for decomposing instances either.
